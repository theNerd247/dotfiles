with (import ../pkgs); 

builtins.mapAttrs (_: x: (mkNixOs x).config.system.build.isoImage)
(rec {
  iso = [ ./modules/isoInstall.nix ./modules/default.nix ];
  vbox = [ ./modules/vboxdemo.nix ] ++ iso;
})
