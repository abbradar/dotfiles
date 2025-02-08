{
  lib,
  config,
  pkgs,
  ...
}: {
  programs.git = {
    enable = true;
    userName = "Nikolay Amiantov";
    userEmail = "ab@fmap.me";
    lfs.enable = true;
    diff-highlight.enable = true;
    extraConfig = {
      rerere.enabled = true;
      push.default = "simple";
      pull.rebase = true;
      init.defaultBranch = "master";
    };
  };

  programs.mercurial = {
    enable = true;
    userName = "Nikolay Amiantov";
    userEmail = "ab@fmap.me";
  };

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting

      set -x EDITOR emacs
      # Too heavyweight for emacs
      set -x GIT_EDITOR vim
      set -x WINEARCH win32
      set -x BROWSER firefox
    '';
  };

  programs.command-not-found = {
    enable = true;
  };

  # We expect this repo to be cloned to .config/home-manager
  home.activation = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run ln -s $VERBOSE_ARG "$HOME/.config/home-manager/.config/nvim" "$HOME/.config/nvim"
    run ln -s $VERBOSE_ARG "$HOME/.config/home-manager/.doom.d" "$HOME/.doom.d"
  '';

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "24.11";
}
