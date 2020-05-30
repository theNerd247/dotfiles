rec {
  version = "20.03";

  nixpkgs = builtins.fetchTarball 
  { url    = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  };

  home-manager = builtins.fetchGit 
  { url = "https://github.com/rycee/home-manager.git";
    ref = "release-${version}";
  };
}
