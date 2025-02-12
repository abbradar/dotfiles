{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [./home-relative-links.nix ./dconf.nix];

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

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      nvim-lspconfig
      nvim-treesitter.withAllGrammars
      packer-nvim
    ];
  };

  # We expect this repo to be cloned to .config/home-manager
  home.relativeLinks = {
    ".config/nvim" = ".config/home-manager/.config/nvim";
    ".doom.d" = ".config/home-manager/.doom.d";
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "24.11";
}
