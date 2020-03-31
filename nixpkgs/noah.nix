{ pkgs, ...}:

{ home       = if pkgs.system == "x86_64-darwin" then "/Users/noah" else "/home/noah";
  shell      = pkgs.fish;
}
