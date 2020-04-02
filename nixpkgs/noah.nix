{ pkgs, ...}:

{ home       = if pkgs.stdenv.isDarwin then "/Users/noah" else "/home/noah";
  shell      = pkgs.fish;
}
