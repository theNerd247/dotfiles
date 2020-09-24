{ modulesPath, lib, ... }: 

{ imports = 
  [ "${modulesPath}/installer/virtualbox-demo.nix"
  ];

  # Let demo build as a trusted user.
  nix.trustedUsers = [ "noah" ];
}
