{ config, pkgs, ...}:

{ users.users.noah = 
    { home  = if pkgs.stdenv.isDarwin then "/Users/noah" else "/home/noah";
      shell = pkgs.fish;
    };

  home-manager.users.noah = import ./home.nix;
}
