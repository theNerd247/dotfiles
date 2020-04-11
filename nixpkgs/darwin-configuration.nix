{ config, ... }:

# TODO:
# [ ] fix hack where fish path puts user profile before system path
# [ ] make vim config more system dependent 
# [ ] refactor vim config for plugins
# [ ] add installing custom fish functions
# [ ] finish platform independent script for installing nix-darwin etc.
# [ ] integrate nixos config (xmonad etc.)

let
  nixversion = "19.09";
  nixplatform = if pkgs.stdenv.isDarwin then "nix-darwin" else "nixos";

  # use darwin config
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "release-${nixversion}";
  };

  fetchChannel = revAndSHA: 
    builtins.fetchTarball 
      { inherit (revAndSHA) sha256; 
        url="https://github.com/NixOS/nixpkgs/archive/${revAndSHA.rev}.tar.gz"; 
      };

  mkChannel = fetchChannel; #) { inherit config; };

  pinnedPkgs = mkChannel
    { rev = nixversion;
      sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
    };

  pkgs = import pinnedPkgs { inherit config; };
in

{
  imports = 
    [ (import "${home-manager}/nix-darwin") 
      (import ./noah.nix)
    ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.nixpkgs/darwin-configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  #nix.package = pinnedPkgs;
  nix.nixPath = "https://github.com/NixOS/nixpkgs/archive/${nixversion}.tar.gz";

  nix.extraOptions = ''
    build-users-group = nixbld
    system-features = benchmark big-parallel local nixos-test
  '';

  nix.binaryCaches = [
    "http://hydra.mv.awakenetworks.net:5000"
  ];

  nix.binaryCachePublicKeys = [
    "hydra.mv.awakenetworks.net:ZfTJ89M1N5ndpH8wJBsMzmiZ20d29oHN8ql/X63NQC8="
  ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  # programs.zsh.enable = true;
  programs.fish.enable = true;

  environment.shells = [
    pkgs.fish
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = if pkgs.stdenv.isDarwin then 10 else 1;
  nix.buildCores = if pkgs.stdenv.isDarwin then 10 else 1;

  home-manager.useUserPackages = false;
}
