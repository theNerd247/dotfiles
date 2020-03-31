{ config, pkgs, ... }:

# TODO list / nix-darwin wish list
# TODO: can some of this be automated with activation hooks?
# - [x] sudo chsh -s $(which bash) py
# - [x] home-manager https://github.com/rycee/home-manager
# - [x] xcode-select --install # Consider https://github.com/xcodereleases/xcinfo
# - [x] Show day of the week in Clock preferences
# - [x] Hide notification preview for Messages
# - [x] Hide notification preview for Slack
# - [x] SSH keys https://help.github.com/articles/generating-an-ssh-key
# - [x] GPG keys https://help.github.com/articles/generating-a-gpg-key
# - [x] Install 1Password from the App Store
# - [x] Install Brave https://brave.com
# - [x] Install ColdBrew https://github.com/keith/ColdBrew
# - [x] Install DiffMerge (Installer not DMG) https://sourcegear.com/diffmerge/downloads.php
# - [x] Install Spark Mail (if using Microsoft accounts) from the App Store
# - [x] Install Slack from the App Store
# - [x] Install a window manager (Flexiglass?) https://ianyh.com/amethyst/
# - [x] Replace Siri button with screen saver button on touch bar
# - [?] ssh agent
# - [x] Install PragmataPro https://www.fsd.it/my-account/downloads
# - [x] iTerm2
# - [x] Show battery percentage in status bar
# - [x] Add home folder to favorites in Finder
# - [x] Show items in a list in Finder
# - [ ] Install Tweetbot
# - [x] Require password immediately after sleep or screen saver begins
# - [x] Show volume in menu bar
# - [x] System Preferences > Keyboard > Shortcuts > Full Keyboard Access (Control+F7)
# - [x] Finder > Preferences > Advanced > Show all filename extensions
# - [x] New Finder window shows: home directory
# - [x] Show Bluetooth in menu bar
# - [x] System Preferences -> Trackpad -> Point & Click -> uncheck "Look up & data detectors"
# - [x] Add Desktop to Dock next to Downloads and "Display as Stack"

{
  imports = [
    <home-manager/nix-darwin>
  ];

  users.users.py = {
    name = "py";
    home = "/Users/py";
  };

  home-manager.users.py = { config, lib, options }: {
    # Can't do this because emacs needs to be able to write to .emacs.d
    /*
    home.file.".emacs.d".source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "v0.200.13";
      sha256 = "0m634adqnwqvi8d7qkq7nh8ivfz6cx90idvwd2wiylg4w1hly252";
    };
    */

    home.file.".gitignore_global".text = ''
      .DS_Store
      .projectile
    '';

    home.file.".gitconfig".text = ''
      [user]
        name = Paul Young
        email = 84700+paulyoung@users.noreply.github.com
      [color]
        branch = auto
        diff = auto
        status = auto
      [color "branch"]
        current = yellow reverse
        local = yellow
        remote = green
      [color "diff"]
        meta = yellow bold
        frag = magenta bold
        old = red bold
        new = green bold
      [color "status"]
        added = yellow
        changed = green
        untracked = cyan
      [commit]
        gpgsign = true
      [core]
        excludesfile = ~/.gitignore_global
      [difftool "diffmerge"]
        cmd = diffmerge $LOCAL $REMOTE --nosplash
      [merge]
        tool = diffmerge
      [mergetool "diffmerge"]
        #cmd = diffmerge.sh --merge --result=$MERGED $LOCAL $BASE $REMOTE
        cmd = "diffmerge --nosplash --merge --result=\"$MERGED\" \"$LOCAL\" \"$(if test -f \"$BASE\"; then echo \"$BASE\"; else echo \"$LOCAL\"; fi)\" \"$REMOTE\""
        trustExitCode = false
      [mergetool]
        keepBackup = false
      [push]
        default = nothing
    '';

    home.file.".ssh/config".text = ''
      Host *
        AddKeysToAgent yes
        IgnoreUnknown UseKeychain
        UseKeychain yes
        IdentityFile ~/.ssh/id_rsa

      Include ~/.ssh/nix-linuxkit-ssh-config
      # Host nix-linuxkit
      #   HostName localhost
      #   User root
      #   Port 24083
      #   IdentityFile /Users/py/.cache/nix-linuxkit-builder/keys/client
      #   StrictHostKeyChecking yes
      #   UserKnownHostsFile /Users/py/.cache/nix-linuxkit-builder/keys/known_host
      #   IdentitiesOnly yes
    '';

    # home.file.".yabairc".text = ''
    # '';

    home.activation.installLinuxKitBuilder = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      nix-env -i /nix/store/jgq3savsyyrpsxvjlrz41nx09z7r0lch-linuxkit-builder
    '';

    home.activation.configureLinuxKit = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      rm -rf ~/.cache/nix-linuxkit-builder
      nix-linuxkit-configure

      # sudo cat /var/root/.ssh/nix-linuxkit-ssh-config
      sudo cp /var/root/.ssh/nix-linuxkit-ssh-config ~/.ssh
      sudo chmod 644 ~/.ssh/nix-linuxkit-ssh-config
      sudo chown py ~/.ssh/nix-linuxkit-ssh-config

      # Run this manually to verify that things work:
      # nix-build /Users/py/.cache/nix-linuxkit-builder/example.nix

      # Try to run the following manually if things don't work
      # launchctl start org.nix-community.linuxkit-builder
    '';

    home.activation.linkDotEmacsDotD = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      #ln -sf $HOME/projects/syl20bnr/spacemacs/v0.200.13 $HOME/.emacs.d
      #ln -sf $HOME/projects/syl20bnr/spacemacs/master $HOME/.emacs.d
      ln -sf $HOME/projects/syl20bnr/spacemacs/develop $HOME/.emacs.d
    '';

    # From Dropbox for now, until it's in git
    home.activation.linkDotSpacemacs = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      ln -sf $HOME/Dropbox/dotfiles/.spacemacs $HOME/.spacemacs
    '';

    home.activation.setUpSsh = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      eval "$(ssh-agent -s)"
      ssh-add -K ~/.ssh/id_rsa
    '';

    # home.activation.installYabaiScriptingAddition = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #   sudo yabai --install-sa
    # '';
  };


  system.defaults.finder.AppleShowAllExtensions = true;

  system.defaults.trackpad.Clicking = true;
  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  environment.shellAliases = {
    emacsclient = "emacsclient --no-wait";
    nix-build = "nix-build --no-out-link";
  };

  environment.shells = [
    pkgs.bashInteractive
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
      # (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865
      # pkgs.cachix

      # (import (builtins.fetchTarball "https://github.com/nix-community/linuxkit-nix/archive/v0.1.4.tar.gz") {}).linuxkit-builder

      #pkgs._1password # some CLI
      pkgs.emacs
      pkgs.jq
      pkgs.git
      pkgs.gnupg
      # pkgs.iterm2 # requires full Xcode
      pkgs.nix-bash-completions
      pkgs.nix-prefetch-git
      pkgs.silver-searcher
      pkgs.vim

      # (pkgs.stdenv.mkDerivation {
      #   name = "yabai-v2.2.2";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "koekeishiya";
      #     repo = "yabai";
      #     rev = "v2.2.2";
      #     sha256 = "0379ppvzvz0hf0carbmyp4rsbd8lipc1g2dwk5324q7iypfv3yrp";
      #   };
      #   buildInputs = with pkgs.darwin.apple_sdk.frameworks; [
      #     Carbon
      #     Cocoa
      #     CoreServices
      #     IOKit
      #     ScriptingBridge
      #     # SkyLight
      #   ];
      #   buildPhase = ''
      #     make install
      #   '';
      #   installPhase = ''
      #     mkdir -p $out/bin
      #     cp -R bin/yabai $out/bin/yabai
      #   '';
      # })

      # unsupported
      #pkgs.dropbox
      #pkgs.google-chrome
      #pkgs.slack
    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient --no-wait";
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient";
  # environment.variables.EDITOR = "${pkgs.emacs}/bin/emacs";
  #environment.variables.EDITOR = "${pkgs.emacs}/Applications/Emacs.app/Contents/MacOS/bin/emacsclient";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.bash.enableCompletion = true;
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 12;
  nix.buildCores = 1;

  nixpkgs.config.allowUnfree = true;

  # "https://cache.nixos.org/" is included, so no need to specify that
  nix.binaryCaches = [
    "http://hydra.mv.awakenetworks.net:5000"
  ];

  # cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=, so no need to specify that
  nix.binaryCachePublicKeys = [
    "hydra.mv.awakenetworks.net:ZfTJ89M1N5ndpH8wJBsMzmiZ20d29oHN8ql/X63NQC8="
  ];

  nix.extraOptions = ''
    system-features = local
  '';
}