{ nixversion }: { config, pkgs, ... }:

{

  home.sessionVariables = 
    { EDITOR="vim";
    };

  # Let Home Manager install and manage itself.
  programs.home-manager =
    { enable = true;
    };

  programs.fish =
    { enable = true;
      shellInit = ''
        for p in /run/current-system/sw/bin /Users/noah/.nix-profile/bin
          if not contains $p $fish_user_paths
            set -g fish_user_paths $p $fish_user_paths
          end
        end
      '';
    };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = nixversion;

  programs.git =
  { enable = true;
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
  { enable  = true;
    extraConfig = builtins.readFile ./init.vim;
    plugins = with pkgs.vimPlugins;
      [ fugitive
        easy-align
      ];
  };
}
