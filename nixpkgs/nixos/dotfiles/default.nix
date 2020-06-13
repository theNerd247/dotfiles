{ pkgs, ... }:

{ nixpkgs.overlays =
  [ (self: super: { noah-dotfiles = super.callPackage ./dotfiles.nix {}; })
  ];

  environment.etc.nixpkgs =
  { enable  = true;
    target  = "nixos";
    source  = pkgs.dotfiles;
    mode    = "660";
  };
}
