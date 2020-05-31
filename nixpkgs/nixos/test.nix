let
  pkgs = import (import ./nixpkgs.nix).nixpkgs 
  { overlays = 
    [ (import ./install-linux.nix)
    ];
  };
in
pkgs.deploy

