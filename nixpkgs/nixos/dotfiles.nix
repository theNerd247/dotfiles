{ stdenv }: 

stdenv.mkDerivation
{ srcs = ./../..
  installPhase = 
  '' mkdir -p $out/
    cp -r ./nixpkgs $out/nixpkgs
  '';
}
