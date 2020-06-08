{ stdenv }: 

stdenv.mkDerivation
{ srcs = builtins.filterSource (path: type: baseNameOf path != "iso" ) ./../..;
  name = "dotfiles";
  installPhase = 
  '' mkdir -p $out/
    cp -r ./nixpkgs $out/nixpkgs
    cp -r .git $out/.git
    cp -r .gitignore $out/.gitignore
  '';
}
