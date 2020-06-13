{ config, overlays, ...}:

rec 
{ fetchChannel = revAndSHA: 
    builtins.fetchTarball 
      { sha256 = revAndSHA.sha256; 
        url="https://github.com/NixOS/nixpkgs/archive/${revAndSHA.rev}.tar.gz"; 
      };

  pinnedPkgs_1909 = 
    { rev = "19.09";
      sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
    };

  pinnedPkgs_2003 =
    { rev = "20.03";
      sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
    };

  pinnedPkgs = pinnedPkgs_2003;

  version = pinnedPkgs.rev;

  # use darwin config
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "release-${nixversion}";
  };

  path = fetchChannel pinnedPkgs;

  nixpkgs = import path;

  nixosPath = "${path}/nixos";

  nixos =  import nixosPath;
}
