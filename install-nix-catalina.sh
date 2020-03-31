#!/usr/bin/env bash

set -euo pipefail

dry_run="false"
total_steps=12
checkpoint_file="$HOME/.install-nix-catalina-checkpoint"

_log() {
  local msg="$1"
  echo "$msg" >&2
}

_error() {
  local msg="$1"
  _log "$msg"
  exit 1
}

_step() {
  local step="$1"
  local msg="$2"
  if [ "$step" != "1" ]; then
    _log ""
    _log "----------------------------------------------------------------------"
    _log ""
  fi
  _log "[$step/$total_steps] $msg"
}

_run() {
  local cmd="$1"
  if [ "$dry_run" != "true" ]; then
    eval "$cmd"
  else
    _log "$cmd (dry run)"
  fi
}

_run_optional() {
  local cmd="$1"
  read -p "This step is optional. Would you like to skip? [yN] " -n 1 -r < /dev/tty
  if echo "$REPLY" | grep --quiet --extended-regexp '^[Yy]$'; then
    _log "Skipping"
  else
    _run "$cmd"
    REPLY=""
  fi

}

do_check_os() {
  if [ "$(uname)" != "Darwin" ]; then
    _error "This script only works on macOS"
  fi

  if sw_vers -productVersion | grep --quiet --invert-match --extended-regexp '^10.15'; then
    _error "You're on an older version of macOS. This script only works on macOS Catalina (10.15)"
  fi
}

do_acquire_sudo() {
  _log "Acquiring administrator privileges using 'sudo'"
  _run "sudo true" \
  || _error "Failed to acquire permissions"
}

do_remove_old_nix() {
  local relocated_path="/Users/Shared/Relocated Items/Security/nix"
  local plist_path="/Library/LaunchDaemons/org.nixos.nix-daemon.plist"

  if [ -f "$plist_path" ]; then
    _log "Unloading nix-daemon"
    _run "sudo launchctl unload '$plist_path'" \
    || _error "Failed to unload nix-daemon"

    _log "Removing nix-daemon .plist file"
    _run "sudo rm -f '$plist_path'" \
    || _error "Failed to remove nix-daemon .plist file"
  fi

  if [ -d "$relocated_path" ]; then
    _log "Removing old Nix installation at '$relocated_path'"
    _run_optional "sudo rm -rf '$relocated_path'" \
    || _error "Failed to remove old Nix installation"
  fi
}

do_synthetic_conf() {
  local synthetic_path="/etc/synthetic.conf"
  local run_symlink_path="/System/Volumes/Data/private/var/run/"
  _run "printf 'nix\nrun\t$run_symlink_path\n' | sudo tee -a $synthetic_path >/dev/null" \
  || _error "Failed to add 'nix' and 'run' to '$synthetic_path'"
}

do_reboot_1() {
  echo "1" > "$checkpoint_file"
  exit 0
}

check_nix_volume_name() {
  diskutil info /nix 2>&1 | grep --quiet "Volume Name" | grep --quiet "Nix"
}

check_nix_volume_exists() {
  diskutil info /nix >/dev/null 2>&1
}

do_apfs_volume() {
  if check_nix_volume_exists; then
    if check_nix_volume_name; then
      _log "Nix APFS volume already created; skipping"
    else
      _error "Nix APFS volume already created but does
not have the volume name 'Nix' associated with it. You can either rename it
using:

  diskutil rename <device> <volume-name>

or delete it using

  diskutil apfs deleteVolume <device>

where <device> is something like 'disk1s6' etc. See 'man diskutil'
for more details on these commands.
"
    fi
  else
    _run "sudo diskutil apfs addVolume disk1 APFSX Nix -mountpoint /nix" \
    || _error "Failed to add volume"
  fi
}

do_fstab() {
  if sudo cat /etc/fstab | grep --quiet "LABEL=Nix"; then
    _log "Nix APFS volume already added to /etc/fstab; skipping"
  else
    _run "echo 'LABEL=Nix /nix apfs rw' | sudo tee -a /etc/fstab > /dev/null" \
    || _error "Failed to add '/nix' to '/etc/fstab'"
  fi
}

do_encrypt() {
  if diskutil info /nix | grep --quiet "FileVault" | grep --quiet "Yes"; then
    _log "Nix APFS volume already encrypted; skipping"
  else
    _log "\
Please choose a strong password, and save it in a password manager. You will \
be required to enter it after rebooting, but you'll be able to save it in \
the macOS Keychain to avoid having to type it in on every reboot."
    _run "diskutil ap encryptVolume Nix -user disk" \
    || _error "Failed to encrypt volume"
  fi
}

do_install() {
  _run "sh <(curl --location https://nixos.org/nix/install) --daemon" \
  || _error "Failed to install Nix"
}

do_reboot_2() {
  echo "2" > "$checkpoint_file"
  exit 0
}

do_configure_nix() {
  local config_source_path=""
  config_source_path=$(mktemp)
  local config_destination_path="/etc/nix/nix.conf"
  cat <<EOF > "$config_source_path"
build-users-group = nixbld
system-features = benchmark big-parallel local nixos-test
substituters = https://cache.nixos.org/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF

  if [ -f "$config_destination_path" ] && [ "$(sudo cat $config_destination_path)" != "build-users-group = nixbld" ]; then
    _log "It looks like you have an existing Nix configuration at $config_destination_path"
    _log "We'll leave it alone, but make sure you aren't missing any of the following settings:"
    _log ""
    cat "$config_source_path"
    _log ""
  else
    _run "cat $config_source_path | sudo tee $config_destination_path >/dev/null" \
    || _error "Failed to configure Nix"
    _run "rm -f /tmp/nix.conf"
  fi
}

do_restart_daemon() {
  _log "Restarting nix-daemon"
  _run "sudo launchctl stop org.nixos.nix-daemon" \
  || _error "Failed to stop nix-daemon"
  _run "sleep 3"
  _run "sudo launchctl start org.nixos.nix-daemon" \
  || _error "Failed to start nix-daemon"
}

do_remove_dylib() {
  local nix_store_path="/nix/store/46qj01lcjz48wb4hc7mvn3mhb3syda7h-Libsystem-osx-10.11.6"
  if [ -e "$nix_store_path" ]; then
    _run "sudo rm -rf $nix_store_path" \
    || _error "Failed to remove 'Libsystem-osx-10.11.6' dylib"
  fi
}

do_finish() {
  rm -f "$checkpoint_file" \
  || _error "Failed to remove checkpoint file at '$checkpoint_file'"

  if [ -d "/Users/Shared/Relocated Items/Security/nix" ]; then
    _log "Don't forget to manually migrate your old Nix installation from '/Users/Shared/Relocated Items/Security/nix'"
  fi
}

main() {
  if [ -f "$checkpoint_file" ]; then
    checkpoint=$(cat "$checkpoint_file" || _error "Failed to read checkpoint file at '$checkpoint_file'")
  else
    checkpoint=0
  fi

  if [ "$checkpoint" = "0" ]; then

    _step 1 "Checking that you're running macOS Catalina (10.15)"
    do_check_os

    do_acquire_sudo

    _step 2 "Checking for old Nix installation"
    do_remove_old_nix

    _step 3 "Adding 'nix' to '/etc/synthetic.conf'"
    do_synthetic_conf

    _step 4 "Please reboot, then run this script again to continue installing"
    do_reboot_1

  elif [ "$checkpoint" = "1" ]; then

    do_acquire_sudo

    _step 5 "Adding APFS volume for '/nix'"
    do_apfs_volume

    _step 6 "Adding '/nix' to '/etc/fstab'"
    do_fstab

    _step 7 "Encrypting Nix APFS volume"
    do_encrypt

    _step 8 "Please reboot, then run this script again to continue installing"
    do_reboot_2

  elif [ "$checkpoint" = "2" ]; then

    do_acquire_sudo

    _step 9 "Installing multi-user Nix"
    do_install

    _step 10 "Configuring Nix"
    do_configure_nix

    _step 11 "Restarting nix-daemon"
    do_restart_daemon

    _step 12 "Removing 'Libsystem-osx-10.11.6' dylib (doesn't work with Catalina)"
    do_remove_dylib

    _log "You're done!"
    do_finish

  fi
}

main

