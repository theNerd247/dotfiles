{ config, pkgs, ... }:

let
  nixpkgs = import ./nixpkgs.nix;

  hardwareConfig = 
    if builtins.pathExists ./hardware-configuration.nix 
    then [ ./hardware-configuration.nix ] 
    else [];
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
