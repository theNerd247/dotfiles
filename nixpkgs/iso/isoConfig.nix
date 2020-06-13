{lib, ...}:  with lib;

{ isoImage.edition = "xmonad";

  networking.wireless.enable = mkForce false;

  # this might re-enable the tty7?
  services.mingetty.autologinUser = mkForce null;
};
