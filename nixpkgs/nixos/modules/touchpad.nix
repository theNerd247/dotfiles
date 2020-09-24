{ lib, ... }:

{ 
  services.xserver = {

    libinput.enable = lib.mkForce false;

    synaptics =
    {
      enable = true;
      twoFingerScroll = true;
      scrollDelta = -75;
    };
  };
}
