args:

let

  fetchPkgs = { rev, sha256 }:
    builtins.fetchTarball 
      { inherit sha256;
        url="https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"; 
      };

  
  pinned =
    { pkgs_1909 = 
        { rev = "19.09";
          sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
        };

      pkgs_2003 =
        { rev = "20.03";
          sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
        };
    }.pkgs_2003;
in

import (fetchPkgs pinned) (args // 
{ overlays = args.overlays ++ 
  [ (self: super:
      { 
        nixpkgsVersion = pinned.rev;
        mkNixOs = imports: self.nixos
          { inherit imports;
          };
       }
  )];
})
