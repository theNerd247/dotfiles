import ./nixpkgs.nix
{ overlays = 
  [ 
    (import ./install-nixos.nix)
    (import ./movie-screen.nix)
  ];
}
