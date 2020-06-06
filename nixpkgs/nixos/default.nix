let 
  nixpkgs = (import ./nixpkgs.nix).nixpkgs;
  nixos = import "${nixpkgs}/nixos"; 

  defaultImports = [ ./configuration.nix ]; 
in
{ imports ? defaultImports }: 

nixos
{ system = "x86_64-linux";
  configuration = 
  { inherit imports;
    nix.nixPath = ["nixpkgs=${nixpkgs}"];
  };
}
