{ config, pkgs, lib, ... }:

{
#  imports = 
#  [ <nixpkgs/nixos/modules/installer/virtualbox-demo.nix> 
#  ];

  # Let demo build as a trusted user.
  nix.trustedUsers = [ "demo" ];

  # Mount a VirtualBox shared folder.
  # This is configurable in the VirtualBox menu at
  # Machine / Settings / Shared Folders.
  #fileSystems."/mnt" = {
  #  fsType = "vboxsf";
  #  device = "noah";
  #  options = [ "rw" ];
  #};
}
