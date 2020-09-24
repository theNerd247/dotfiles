self: super:

{ nhdotfiles = super.stdenv.mkDerivation
    { srcs = builtins.filterSource (path: type: baseNameOf path != "result" ) ./../..;
      name = "nhdotfiles";
      installPhase = 
      '' mkdir -p $out

        install --group=wheel --mode=770 -D ./bin/install $out/bin/install
      '';
    };
}
