{ config, pkgs, ... }:

let
  nixpkgs = import ./nixpkgs.nix;
in

{
  imports = 
  [ # ./vboxdemo.nix
    (import "${nixpkgs.home-manager}/nixos")
    (import ../noah.nix)
    (import ./hardware.nix)
  ];
  
  system.stateVersion = nixpkgs.version;

  networking.networkmanager = 
  { enable = true;
    dns = "none";
    insertNameservers = ["185.228.168.168"];
  };

  environment.systemPackages = with pkgs; [
    vim
    git
  ];

  services.xserver = 
  { enable = true;

    displayManager = 
    { sddm.enable = true;
    };

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

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
}
