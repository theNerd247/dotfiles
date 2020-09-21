{ config, ... }:

{ imports = 
    [ ./home-manager
      ./noah
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

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  # programs.zsh.enable = true;
  programs.bash = 
  { enable = true;
    enableCompletion = true;
  };

  environment.shells = [
    pkgs.bash
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = if pkgs.stdenv.isDarwin then 10 else 1;
  nix.buildCores = if pkgs.stdenv.isDarwin then 10 else 1;

}
