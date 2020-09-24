args:

let
  pinned = import ./channels/20_03.nix;
in

import (builtins.fetchGit pinned) (args // 
{ overlays = args.overlays ++ 
  [ (self: super:
      { 
        nixpkgsVersion = pinned.ref;
        mkNixOs = imports: self.nixos
          { inherit imports;
          };
       }
  )];
})
