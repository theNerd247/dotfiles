{pkgs, ...}:

let
  diffconflicts = pkgs.vimUtils.buildVimPluginFrom2Nix
  rec
  { pname = "diffconflicts";
    version = "2.2.0";
    src = builtins.fetchTarball 
    { url = "https://github.com/whiteinge/diffconflicts/archive/${version}.tar.gz";
      sha256 = "0gb5rmgsnx5a7f8rilmm95blqq07fhajinnd4n4g5gy1sa16hy2a";
    };
  };

in

{ programs =
  { vim = 
    { plugins = [ diffconflicts ];
    };

    git =
    { extraConfig =
      { merge.tool = "diffconflicts";
        mergetool =
        { diffconflicts =
          { cmd = ''vim -c DiffConflicts "$MERGED" "$BASE" "$LOCAL" "$REMOTE"'';
            trustExitCode = true;
            keepBackup = false;
          };
        };
      };
    };
  };
}
