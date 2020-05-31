{ config, pkgs, ... }:

let
  nixpkgs = import ./nixpkgs.nix;

  hardwareConfig = 
    if builtins.pathExists ./hardware-configuration.nix 
    then [ ./hardware-configuration.nix ] 
    else [];
in

{
  imports = 
  [ "${nixpkgs.home-manager}/nixos"
    ../noah.nix
  ] ++ hardwareConfig;

  nixpkgs.overlays = 
  [ (import ./install-linux.nix)
  ];

  boot.loader.systemd-boot.enable = true;
  
  system.stateVersion = nixpkgs.version;

  # networking.networkmanager = 
  # { enable = true;
  #   dns = "none";
  #   insertNameservers = ["185.228.168.168"];
  # };

  environment.systemPackages = with pkgs; [
    vim
    git
  ];

  services.xserver = 
  { enable = true;

    displayManager = 
    { sddm.enable = true;
    };

    #desktopManager.plasma5.enable = false;

    windowManager =
    { xmonad = 
      { enable = true;
        enableContribAndExtras = true;
        config = builtins.readFile ./xmonad.hs;
      };
    };
  };

  home-manager.useUserPackages = false;
  home-manager.useGlobalPkgs = true;

  users.mutableUsers = false;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
}
