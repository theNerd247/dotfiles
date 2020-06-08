{ pkgs }:

{ nixpkgs.overlays =
  [ (self: super: self.callPackage ./dotfiles.nix {};
  ];

  environment.etc.nixpkgs =
  { enabled = true;
    target  = "nixos/";
    source  = pkgs.dotfiles;
    mode    = "660";
  };
}
