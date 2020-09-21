{ modulesPath, lib, ... }: 

{ imports = 
  [ "${modulesPath}/installer/virtualbox-demo.nix"
  ];

  services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
  services.xserver.displayManager.sddm.enable = lib.mkForce false;

  # Let demo build as a trusted user.
  nix.trustedUsers = [ "noah" ];
}
