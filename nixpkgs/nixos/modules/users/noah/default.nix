{ pkgs, ... }:

{ users.users.noah =
    { home  = "/home/noah";
      shell = pkgs.bash;
      createHome = true;
      extraGroups = [ "wheel" ]; 
      initialHashedPassword = 
        pkgs.lib.strings.escapeNixString
        "$6$4L//aDSh4X2T62c$4fzzfUYmW.Uav8cY8kLA9uNtCunzoQEh8lU8tsqVbwPubqba6Sj4WNGRWSH7zTclMCddJCe.Z7aTvWrdAv0vn1";
    };

  home-manager.users.noah = import ./home.nix;
}
