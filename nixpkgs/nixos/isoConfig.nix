{ lib, ...}:  with lib;

{ isoImage.edition = "xmonad";

  networking.wireless.enable = mkForce false;
};
