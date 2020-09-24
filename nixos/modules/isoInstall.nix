{ modulesPath, ...}: 

{ 
  imports =
  ["${modulesPath}/installer/cd-dvd/installation-cd-base.nix"
  ];

  isoImage.edition = "xmonad";
}
