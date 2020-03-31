{ config, pkgs, ... }:

# TODO:
# [ ] fix hack where fish path puts user profile before system path
# [ ] make vim config more system dependent 
# [ ] refactor vim config for plugins
# [ ] add installing custom fish functions
# [ ] finish platform independent script for installing nix-darwin etc.
# [ ] integrate nixos config (xmonad etc.)

let
  nixversion = "19.09";
  nixplatform = if pkgs.system == "x86_64-darwin" then "nix-darwin" else "nixos";

  # use darwin config
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "release-${nixversion}";
  };
in

{
  imports = [ (import "${home-manager}/nix-darwin") ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  nix.extraOptions = ''
    build-users-group = nixbld
    system-features = benchmark big-parallel local nixos-test
  '';
#     substituters = http://hydra.mv.awakenetworks.net:5000 https://cache.nixos.org/
#     trusted-public-keys = hydra.mv.awakenetworks.net:ZfTJ89M1N5ndpH8wJBsMzmiZ20d29oHN8ql/X63NQC8= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
#   '';

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
  nix.maxJobs = if pkgs.system == "x86_64-darwin" then 10 else 1;
  nix.buildCores = if pkgs.system == "x86_64-darwin" then 10 else 1;

  home-manager.useUserPackages = false;

  users.users.noah = import ./noah.nix { inherit pkgs; };

  home-manager.users.noah = import ./home.nix { inherit nixversion; };
}
