{ pkgs, ... }:

{ environment.etc.nhdotfiles =
  { 
    enable  = true;
    target  = "nhdotfiles";
    source  = pkgs.nhdotfiles;
    mode    = "660";
  };
}
