{ pkgs, ... }:

let
  home-manager = builtins.fetchGit
    { url = "https://github.com/rycee/home-manager.git";
      ref = "release-20.03"; #${pkgs.nixpkgsVersion}";
    };
in

{ imports = [ "${home-manager}/nixos" ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
}
