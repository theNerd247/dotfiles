{ stdenv }: 

stdenv.mkDerivation
{ srcs = ./../..;
  name = "dotfiles";
  installPhase = 
  '' mkdir -p $out/
    cp -r ./nixpkgs $out/nixpkgs
  '';
}
