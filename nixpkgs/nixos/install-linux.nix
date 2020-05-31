self: super: 

with { inherit (super) writeScriptBin parted bash util-linux; };

rec
{ deploy = writeScriptBin "deploy" 
  ''
    #!${bash}/bin/bash
    nixos-generate-config --show-hardware-config > ./hardware-configuration.nix \
      && nix-build --attr system ./default.nix --show-trace \
      && nix-env --profile /nix/var/nix/profiles/system --set ./result \
      && ./result/bin/switch-to-configuration switch
  '';

  insall-nix = writeScriptBin "install-nix" 
  ''
    #!${bash}/bin/bash
    
    function partitionDisks() {
      ${parted}/bin/parted /dev/sda -- mklabel gpt
      ${parted}/bin/parted /dev/sda -- mkpart primary 512MiB -8GiB
      ${parted}/bin/parted /dev/sda -- mkpart primary linux-swap -8GiB 100%
      ${parted}/bin/parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
      ${parted}/bin/parted /dev/sda -- set 3 boot on
    }
    
    function formatFileSystem() {
    
      ${util-linux}/bin/mkfs.ext4 -L nixos /dev/sda1
    
      ${util-linux}/bin/mkswap -L swap /dev/sda2
    
      ${util-linux}/bin/swapon /dev/sda2
    
      ${util-linux}/bin/mkfs.fat -F 32 -n boot /dev/sda3        # (for UEFI systems only)
    
      ${util-linux}/bin/mount /dev/disk/by-label/nixos /mnt
    
      mkdir -p /mnt/boot                      # (for UEFI systems only)
    
      ${util-linux}/bin/mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)
    }
    
    partitionDisks
    formatFileSystem
    pushd /mnt
    ${deploy}/bin/deploy
  '';
}
