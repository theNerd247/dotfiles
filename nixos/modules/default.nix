{ lib, pkgs, ... }:

{ imports = 
  [ 
    ./users/noah
    ./homeManager.nix
    ./xmonad
    ./touchpad.nix
  ];

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

  # Here we manually set the nixpkgs and nixos-config paths for NIX_PATH of the
  # system. in the install-nixos script we'll use the nhdotfiles (which is just
  # a copy of this repo in the nix-store) store path to clone the cached repo
  # into the indicated install path.
  nix.nixPath = pkgs.nhdotfiles.nixPath;
}
