{ pkgs, ... }:

{ users.users.noah =
    { home  = "/home/noah";
      shell = pkgs.bash;
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" ]; 
      initialHashedPassword = 
        "\$6\$h1CYjiaf3P7U5Fp\$/FEjtPGMggPBqxOybq5GgxHVeHGgdEfs.jz0lwTaBNiosm1a.C3BecujGebgDi1D2MhTCTqPn9vYk5MYcYwSP.";
    };

  home-manager.users.noah = import ./home.nix;
}
