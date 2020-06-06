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

  hardware.pulseaudio = 
  { enable = true;
  };

  environment.systemPackages =  with pkgs;
  [ xbacklight
    termite
    firefox
    zathura
  ];
}
