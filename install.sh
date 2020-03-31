#!/usr/bin/env bash

set -euo pipefail

dry_run="false"
hmBaseURL="https://github.com/rycee/home-manager/archive/"

source ./_lib

nixpksVersion() {
  echo "$nixpkgsVersion"
}

hmVersion() {
  echo "release-$(nixpksVersion)"
}

onDarwin() {
  uname -a | grep --quiet -i "darwin"
}

onNixOS() {
  false;
}

# install multi-user
function installNix() {
  if onDarwin; then
    ./install-nix-catalina.sh
  else
    _log "installing multi-user nix"
    _run "sh <(curl --location https://nixos.org/nix/install) --daemon"
  fi 
}

installNixDarwin() {
  if onDarwin; then
    _log "you're on a mac installing nix-darwin..."
    _run_optional "nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer && ./result/bin/darwin-installer"
  fi
}

installHomeManager() {
  if ! onDarwin && ! onNixOS; then
    # Instructions from standalone installation from https://rycee.gitlab.io/home-manager/

    mkdir -m 0755 -p "/nix/var/nix/{profiles,gcroots}/per-user/$USER"
    nix-channel --add home-manager "${hmBaseURL}/$(hmVersion).tar.gz"
    nix-channel --update

    local shellExport="NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH"

    $shellExport nix-shell '<home-manager>' -A install

    _log "Please add the following to your shell config:"
    _log "$shellExport"
  else
    _log "Skipping home-manager install"
  fi
}

installUserNixpkgs(){
  local nixpkgsDir="$HOME/.nixpkgs"

  if test -d $nixpkgsDir; then
    echo "$nixpkgsDir already exists! Backing it up first..."

    mv $nixpkgsDir "${nixpkgsDir}-backup"
  fi

  ln -sT ./nixpkgs $nixpkgsDir
}

# installNix \
  installNixDarwin \
  && installUserNixpkgs \
  && installHomeManager \
