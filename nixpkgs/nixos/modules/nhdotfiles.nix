{ pkgs, ... }:

{ environment.etc.nhdotfiles =
  { enable  = true;
    target  = "nixos";
    source  = pkgs.nhdotfiles;
    mode    = "660";
  };
}
