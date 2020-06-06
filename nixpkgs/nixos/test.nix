let
  nixpkgs = import ./nixpkgs.nix;

  pkgs = import nixpkgs.nixpkgs 
  { overlays = 
    [ (import ./install-linux.nix)
    ];
  };
in

pkgs.callPackage ./dotfiles.nix {}
