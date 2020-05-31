let
  nixpkgs = (import ./nixpkgs.nix).nixpkgs;
in
import ./default.nix
{ imports = 
  [ "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    ./configuration.nix
  ];
}
