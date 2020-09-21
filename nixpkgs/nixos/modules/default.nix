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
  ]; 
  #++ hardwareConfig;
  #hardwareConfig;

  services.mingetty.autologinUser = lib.mkForce null;

  # environment.systemPackages = with pkgs;
  # [ install-nixos
  # ];

  boot.loader.systemd-boot.enable = true;
  
  #system.stateVersion = pkgs.nixpkgsVersion;

  networking = 
  { networkmanager = 
    { enable = true;
      dns = "none";
      insertNameservers = ["185.228.168.168" "185.228.169.168"];
    };

    wireless.enable = lib.mkForce false;
  };

  users.mutableUsers = false;
}
