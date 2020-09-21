self: super:

{ nhdotfiles = super.stdenv.mkDerivation
    { srcs = builtins.filterSource (path: type: baseNameOf path != "result" ) ./../../..;
      name = "nhdotfiles";
      installPhase = 
      '' mkdir -p $out/
        cp -r ./nixpkgs $out/nixpkgs
        cp -r ./nixos $out/nixos
        cp -r .git $out/.git
        cp -r .gitignore $out/.gitignore
      '';
    };
}
