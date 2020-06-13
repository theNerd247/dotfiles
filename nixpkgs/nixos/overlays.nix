self: super: 

with { inherit (super) writeScriptBin parted bash utillinux noah-dotfiles; };

rec
{ deploy = writeScriptBin "deploy" 
  ''
    #!${bash}/bin/bash
      cd ./etc/nixos \
      && nixos-generate-config --show-hardware-config > ./hardware-configuration.nix \
      && nix-build --attr system ./default.nix --show-trace \
      && nix-env --profile /nix/var/nix/profiles/system --set ./result \
      && ./result/bin/switch-to-configuration switch
  '';

  install-nixos = writeScriptBin "install-nixos" 
  ''
    #!${bash}/bin/bash

    set -xe
    
    function partitionDisks() {
      ${parted}/bin/parted /dev/sda -- mklabel gpt
      ${parted}/bin/parted /dev/sda -- mkpart primary 512MiB -8GiB
      ${parted}/bin/parted /dev/sda -- mkpart primary linux-swap -8GiB 100%
      ${parted}/bin/parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
      ${parted}/bin/parted /dev/sda -- set 3 boot on
    }
    
    function formatFileSystem() {
    
      ${utillinux}/bin/mkfs.ext4 -L nixos /dev/sda1
    
      ${utillinux}/bin/mkswap -L swap /dev/sda2
    
      ${utillinux}/bin/swapon /dev/sda2
    
      ${utillinux}/bin/mkfs.fat -F 32 -n boot /dev/sda3        # (for UEFI systems only)
    
      ${utillinux}/bin/mount /dev/disk/by-label/nixos /mnt
    
      mkdir -p /mnt/boot                      # (for UEFI systems only)
    
      ${utillinux}/bin/mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)
    }
    
    partitionDisks
    formatFileSystem
    pushd /mnt
    ${deploy}/bin/deploy
  '';
}
