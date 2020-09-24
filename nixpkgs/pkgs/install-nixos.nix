self: super:

let
  deploy = super.writeScriptBin "deploy" 
  ''
    #!${self.bash}/bin/bash

    cd ./etc/nixos \
    && nixos-generate-config --show-hardware-config > ${./hardware-configuration.nix} \
    && nix-build --attr system ${./default.nix} --show-trace \
    && nix-env --profile /nix/var/nix/profiles/system --set ./result \
    && ./result/bin/switch-to-configuration switch
  '';

  install-nixos = super.writeScriptBin "install-nixos" 
  ''
    #!${self.bash}/bin/bash

    set -xe
    
    function partitionDisks() {
      ${self.parted}/bin/parted /dev/sda -- mklabel gpt
      ${self.parted}/bin/parted /dev/sda -- mkpart primary 512MiB -8GiB
      ${self.parted}/bin/parted /dev/sda -- mkpart primary linux-swap -8GiB 100%
      ${self.parted}/bin/parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
      ${self.parted}/bin/parted /dev/sda -- set 3 boot on
    }
    
    function formatFileSystem() {
    
      ${self.utillinux}/bin/mkfs.ext4 -L nixos /dev/sda1
      ${self.utillinux}/bin/mkswap -L swap /dev/sda2
      ${self.utillinux}/bin/swapon /dev/sda2
      ${self.utillinux}/bin/mkfs.fat -F 32 -n boot /dev/sda3        # (for UEFI systems only)
      ${self.utillinux}/bin/mount /dev/disk/by-label/nixos /mnt
    
      mkdir -p /mnt/boot                      # (for UEFI systems only)
    
      ${self.utillinux}/bin/mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)
    }
    
    partitionDisks
    formatFileSystem
    pushd /mnt
    ${deploy}/bin/deploy
  '';
in

{ install-nixos = super.symlinkJoin 
  { name = "install-linux";
    paths = [ install-nixos deploy ];
  };
}
