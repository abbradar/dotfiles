{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use chrooted builds.
  nix.useChroot = true;

  # Allow unfree packages.
  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableAdobeFlash = true;
      icedtea = true;
    };

    chromium = {
     enablePepperFlash = true;
     enablePepperPDF = true;
    };

    cabal.libraryProfiling = true;
  };

  boot = {
    # Use the latest kernel version.
    kernelPackages = pkgs.linuxPackages_latest;

    cleanTmpDir = true;

    loader.efi.canTouchEfiVariables = true;
    loader.grub.version = 2;
  };

  # Time zone
  time.timeZone = "Europe/Moscow";

  # Security
  security.sudo.configFile = ''
    Defaults rootpw,insults,timestamp_timeout=60
  '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Packages
  environment = {
    systemPackages = (with pkgs; [
      # Management
      smartmontools
      lm_sensors
      htop
      iotop
      nethogs
      psmisc

      # Files
      p7zip
      zip
      unzip
      unrar
      tree
      rsync

      # Editors
      vim

      # Runtimes
      python3
      ruby_2_1
      jre
      icedtea7_web
      wine
      mono

      # Develompent
      nix-repl
      git
      subversion

      # Networking
      inetutils
      cifs_utils
      nfsUtils
      openvpn
      wget
      miniupnpc

      # Utilities
      screen
    ]);
  };

  # Enable OpenGL support.
  hardware.opengl.driSupport32Bit = true;

  # Zsh with proper path
  programs.zsh.enable = true;
}
