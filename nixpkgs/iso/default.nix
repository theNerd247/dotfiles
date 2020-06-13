{ pkgs, ... }: with (pkgs) nixos nixosPath;

nixos
{ imports = 
  [ "${nixosPath}/modules/installer/cd-dvd/installation-cd-base.nix"
    ../configuration.nix
    ./isoConfig.nix
  ];
}
