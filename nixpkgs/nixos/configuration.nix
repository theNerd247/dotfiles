{ config, pkgs, ... }:

let
  nixpkgs = import ./nixpkgs.nix;

  hardwareConfig = 
    if builtins.pathExists ./hardware-configuration.nix 
    then [ ./hardware-configuration.nix ] 
    else [];

  # TODO: add environment.etc.nixpkgs config to copy (not symlink) the contents
  # of this package. This will allow child systems to derive their own system
  # based off of this one.
  dotfiles = pkgs.callPackage ./dotfiles.nix {};
in

{ imports = 
  [ "${nixpkgs.home-manager}/nixos"
    ../noah.nix
    ./xmonad.nix
  ] ++ hardwareConfig;

  nixpkgs.overlays = 
  [ (import ./install-linux.nix)
  ];

  boot.loader.systemd-boot.enable = true;
  
  system.stateVersion = nixpkgs.version;

  networking.networkmanager = 
  { enable = true;
    dns = "none";
    insertNameservers = ["185.228.168.168"];
  };

  home-manager.useUserPackages = false;
  home-manager.useGlobalPkgs = true;

  users.mutableUsers = false;
}
