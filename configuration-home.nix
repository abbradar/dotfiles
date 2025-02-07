{pkgs, ...}: {
  programs.git = {
    enable = true;
    userName = "Nikolay Amiantov";
    userEmail = "ab@fmap.me";
  };

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting
    '';
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "24.11";
}
