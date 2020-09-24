{ lib, pkgs, ... }:

let
  hardwareConfig = 
    if builtins.pathExists ./hardware-configuration.nix 
    then [ ./hardware-configuration.nix ] 
    else [];
in

{ imports = 
  [ 
    ./users/noah
    ./homeManager.nix
    ./xmonad
    ./touchpad.nix
  ] 
  ++ hardwareConfig;

  environment.systemPackages = with pkgs;
  [ 
    install-nixos
    git
  ];

  boot.loader.systemd-boot.enable = true;
  
  system.stateVersion = pkgs.nixpkgsVersion;

  networking = 
  { networkmanager = 
    { enable = true;
    };

    wireless.enable = lib.mkForce false;
  };

  users.mutableUsers = true;
}
