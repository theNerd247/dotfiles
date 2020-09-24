{ pkgs, ...}:

{ services.xserver = 
  { enable = true;

    windowManager =
    { xmonad = 
      { enable = true;
        enableContribAndExtras = true;
        config = builtins.readFile ./xmonad.hs;
      };
    };
  };

  services.mingetty.autologinUser = lib.mkForce null;
  services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
  services.xserver.displayManager.sddm.enable = lib.mkForce false;

  hardware.pulseaudio = 
  { enable = true;
  };

  environment.systemPackages =  with pkgs;
  [ xorg.xbacklight
    termite
    firefox
    zathura
  ];
}
