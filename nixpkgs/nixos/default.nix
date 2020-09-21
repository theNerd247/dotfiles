with (import ../pkgs); 

{
  iso = (mkNixOs [ ./modules/isoInstall.nix ./modules/default.nix ]).config.system.build.isoImage;
  vbox = (mkNixOs [ ./modules/vboxdemo.nix ./modules/default.nix ]).config.system.build.virtualBoxOVA;
}
