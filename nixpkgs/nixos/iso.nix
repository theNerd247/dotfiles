let
  nixpkgs = (import ./nixpkgs.nix).nixpkgs;
  nixos = import ./default.nix;
  cfg = 
  { lib, ...}:  with lib;

  { isoImage.edition = "xmonad";

    networking.wireless.enable = mkForce false;

    # this might re-enable the tty7?
    services.mingetty.autologinUser = mkForce null;
  };
in
nixos
{ imports = 
  [ "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
    ./configuration.nix
    cfg
  ];
}
