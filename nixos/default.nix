with (import ../pkgs); 

let
  hardwareConfig = 
    if builtins.pathExists ../hardware-configuration.nix 
    then [ ../hardware-configuration.nix ] 
    else [];
in
{
  system = (mkNixOs ([ ./modules/default.nix ] ++ hardwareConfig)).config.system.build;
  iso = (mkNixOs [ ./modules/isoInstall.nix ./modules/default.nix ]).config.system.build.isoImage;
  vbox = (mkNixOs [ ./modules/vboxdemo.nix ./modules/default.nix ]).config.system.build.virtualBoxOVA;
}
