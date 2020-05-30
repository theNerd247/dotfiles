let 
  pkgs = import ./nixpkgs.nix;

  nixosPath = "${pkgs.nixpkgs}/nixos";

  nixos = import nixosPath; 
in

(nixos
{ system = "x86_64-linux";
  configuration =
  { imports =
    [ ./configuration.nix
    ];

    nix.nixPath = ["nixpkgs=${pkgs.nixpkgs}"];
  };
}).system
