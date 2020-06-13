{ pkgs }: with (pkgs) nixos nixpkgsPath;

{ imports }: 

nixos
{ system = "x86_64-linux";
  configuration = 
  { inherit imports;
    nix.nixPath = ["nixpkgs=${nixpkgsPath}"];
    nix.package = pkgs;
  };
}
