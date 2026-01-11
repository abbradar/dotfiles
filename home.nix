{
  lib,
  config,
  pkgs,
  ...
}:
with lib; {
  imports = [./home-relative-links.nix ./dconf.nix];

  xdg.configFile."nixpkgs/config.nix".source = ./.config/nixpkgs/config.nix;

  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        email = "ab@fmap.me";
        name = "Nikolay Amiantov";
      };
      rerere.enabled = true;
      push.default = "simple";
      pull.rebase = true;
      init.defaultBranch = "master";
    };
  };

  programs.diff-highlight = {
    enable = true;
    enableGitIntegration = true;
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
      set -x BROWSER firefox
    '';
  };

  # For environments without fish
  programs.bash = {
    enable = true;

    initExtra = ''
      export EDITOR=emacs
      # Too heavyweight for emacs
      export GIT_EDITOR=vim
      export BROWSER=firefox
    '';
  };

  programs.direnv.enable = true;

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
      lazy-nvim
      coq_nvim
    ];
    # https://github.com/LazyVim/LazyVim/discussions/1972
    extraLuaConfig = let
      plugins = with pkgs.vimPlugins; [
        nvim-lspconfig
        nvim-treesitter.withAllGrammars
        coq_nvim
      ];
      mkEntryFromDrv = drv:
        if lib.isDerivation drv
        then {
          name = "${lib.getName drv}";
          path = drv;
        }
        else drv;
      lazyPath = pkgs.linkFarm "lazy-plugins" (builtins.map mkEntryFromDrv plugins);
    in ''
      local lazyPath = "${lazyPath}";
      ${readFile ./.config/nvim/init.lua}
    '';
  };

  xdg.configFile = {
    "nvim/lua".source = ./.config/nvim/lua;
    "nvim/ginit.vim".source = ./.config/nvim/ginit.vim;
  };

  # We expect this repo to be cloned to .config/home-manager
  home.relativeLinks = {
    ".doom.d" = ".config/home-manager/.doom.d";
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "24.11";
}
