self: super:

rec
{
  install-nhdotfiles = super.writeScriptBin "install-nhdotfiles"
    ''
    #!${self.bash}/bin/bash

    installPath="$1/${nhdotfiles.installPath}"

    ${self.git}/bin/git clone ${nhdotfiles} $installPath \
    && cd $installPath \
    && nixos-generate-config --show-hardware-config > ./hardware-configuration.nix \
    && NIX_PATH="${builtins.concatStringsSep ":" nhdotfiles.nixPath}" nixos-rebuild switch
    '';

  nhdotfiles = super.stdenv.mkDerivation
    rec
    { srcs = builtins.filterSource (path: type: baseNameOf path != "result" ) ./..;
      installPath = "/etc/nhdotfiles";
      nixPath = 
        [
          "nixpkgs=${installPath}/pkgs"
          "nixos-config=${installPath}/nixos/modules/default.nix"
        ];
      name = "nhdotfiles";
      installPhase = 
      '' mkdir -p $out

        cp -r ./. $out
      '';
    };
}
