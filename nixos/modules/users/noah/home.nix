{ pkgs, ...}:

{ 

  home.sessionVariables = 
    { EDITOR = "vim";
    };

  # home.packages = with pkgs;
  #   [ #gitAndTools.gh 
  #   ];

  programs.kakoune =
    { enable = true;
    };

  # Let Home Manager install and manage itself.
  programs.home-manager =
    { enable = true;
    };

  programs.firefox = 
  { enable = ! pkgs.stdenv.isDarwin;
  };

  programs.bash = 
  { enable = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  #home.stateVersion = builtins.trace config.nix.version config.nix.version;

  programs.git =
  { enable    = true;
    userEmail = "noah.harvey247 gm";
    userName  = "theNerd247";
    aliases = 
      { co   = "checkout";
        cm   = "checkout master";
        b    = "branch";
        rb   = "rebase";
        rbi  = "rebase -i";
        lgga = "log --graph --decorate --oneline --all";
        st   = "status";
      };

  }; 

  programs.vim = 
  { enable      = true;
    extraConfig = builtins.readFile ./init.vim;
    plugins     = with pkgs.vimPlugins;
      [ 
        easy-align
      ];
  };

}
