self: super:

{

  # This is how to manually deploy the system...
  # && nix-build --attr system ${../nixos/default.nix} --show-trace \
  # && nix-env --profile /nix/var/nix/profiles/system --set ./result \
  # && ./result/bin/switch-to-configuration switch
  install-nixos = (super.writeScriptBin "install-nixos" 
  ''
    #!${self.bash}/bin/bash

    set -xe

    installDev="$1"

    mntPoint=/mnt

    if [[ -z "$installDev" ]]; then 
      echo "install device required (e.g /dev/sda) as first argument"
      exit 1
    fi

    function partitionDisks() {
      ${self.parted}/bin/parted ''${installDev} -- mklabel gpt
      ${self.parted}/bin/parted ''${installDev} -- mkpart primary 512MiB -4GiB
      ${self.parted}/bin/parted ''${installDev} -- mkpart primary linux-swap -4GiB 100%
      ${self.parted}/bin/parted ''${installDev} -- mkpart ESP fat32 1MiB 512MiB
      ${self.parted}/bin/parted ''${installDev} -- set 3 boot on
    }
    
    function formatFileSystem() {
    
      ${self.utillinux}/bin/mkfs.ext4 -L nixos ''${installDev}1
      ${self.utillinux}/bin/mkswap -L swap ''${installDev}2
      ${self.utillinux}/bin/swapon ''${installDev}2
      ${self.utillinux}/bin/mkfs.fat -F 32 -n boot ''${installDev}3        # (for UEFI systems only)
      ${self.utillinux}/bin/mount /dev/disk/by-label/nixos $mntPoint
    
      mkdir -p ''${mntPoint}/boot                      # (for UEFI systems only)
    
      ${self.utillinux}/bin/mount /dev/disk/by-label/boot ''${mntPoint}/boot # (for UEFI systems only)
    }
    
    partitionDisks \
    && formatFileSystem \
    && ${self.install-nhdotfiles}/bin/install-nhdotfiles $mntPoint 
  '').overrideAttrs (x: x // { buildInputs = with self; [ parted utillinux install-nhdotfiles ]; });
}
