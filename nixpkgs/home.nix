{ config, pkgs, ...}:

let
  neuronGit = builtins.fetchGit 
  { url = "https://github.com/srid/neuron"; 
    ref = "master"; 
  };

  neuron = import neuronGit 
  { inherit pkgs;
    gitRev = neuronGit.shortRev; 
  };
in

{
  home.sessionVariables = 
    { EDITOR = if pkgs.stdenv.isDarwin then "vim" else "nvim";
    };

  home.packages = 
    [ neuron
    ];

  # Let Home Manager install and manage itself.
  programs.home-manager =
    { enable = true;
    };

  #targets.genericLinux.enable = if pkgs.stdenv.isDarwin then false else true;

# TODO: make this work on darwin
# programs.firefox = 
#   { enable = false;
#      extensions = with pkgs.nur.repos.rycee.firefox-addons;
#        [ adblocker-plus
#          vim-vixen
#        ];
#   };

  programs.fish =
    { enable = true;
      # NOTE: This is a hack and needs to be fixed
      # see https://github.com/LnL7/nix-darwin/issues/122
      shellInit = ''
        for p in /run/current-system/sw/bin /Users/noah/.nix-profile/bin
          if not contains $p $fish_user_paths
            set -g fish_user_paths $p $fish_user_paths
          end
        end
      '';
      interactiveShellInit = ''
        fish_vi_key_bindings
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
  #home.stateVersion = builtins.trace config.nix.version config.nix.version;

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
  { enable  = pkgs.stdenv.isDarwin;
    extraConfig = builtins.readFile ./init.vim;
    plugins = with pkgs.vimPlugins;
      [ # fugitive
        easy-align
        # ghcid
      ];
  };

  programs.neovim = 
  { enable  = if pkgs.stdenv.isDarwin then false else true;
    extraConfig = builtins.readFile ./init.vim;
    plugins = with pkgs.vimPlugins;
      [ fugitive
        easy-align
        ghcid
      ];
  };

}
