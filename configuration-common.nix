{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
    };

    # Use chrooted builds.
    useChroot = true;

    #binaryCaches = [ "https://abbradar.net/hydra" ];
    #binaryCaches = [ "http://hydra.cryp.to/" "http://hydra.nixos.org/" ];
    binaryCaches = [ "http://hydra.nixos.org/" ];
    requireSignedBinaryCaches = true;
    binaryCachePublicKeys = [ "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
                            ];
    extraOptions = ''
      auto-optimise-store = true
    '';
  };

  nixpkgs.config = {
    # Allow unfree packages.
    allowUnfree = true;

    firefox = {
      enableAdobeFlash = true;
      icedtea = true;
    };

    chromium = {
     enablePepperFlash = true;
     enablePepperPDF = true;
    };

    packageOverrides = self: with self; {
      my_ruby = ruby_2_2;
      pidgin-with-plugins = pidgin-with-plugins.override {
        plugins = [ pidginlatex pidginotr ];
      };
    };
  };

  boot = {
    # Use the latest kernel version.
    kernelPackages = pkgs.linuxPackages_latest;

    cleanTmpDir = true;

    loader = {
      timeout = 0;
      efi.canTouchEfiVariables = true;
      grub.version = 2;
    };
  };

  # Time zone
  time.timeZone = "Europe/Moscow";

  # Security
  security = {
    sudo.configFile = ''
      Defaults rootpw,insults,timestamp_timeout=60
    '';
    rngd.enable = true;
  };

  # Select internationalization properties.
  i18n = {
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  services = {
    xserver = {
      layout = "ru,us";
      xkbOptions = "eurosign:e,grp:caps_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";
    };

    openssh = {
      passwordAuthentication = false;
    };

    journald.extraConfig = ''
      SystemMaxFileSize=5M
    '';
  };

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
      lsof

      # Partitions
      wipe
      btrfsProgs
      hdparm

      # Files
      gptfdisk
      p7zip
      zip
      unzip
      unrar
      tree
      rsync
      file
      pv
      dos2unix

      # Editors
      vim

      # Runtimes
      python3
      my_ruby
      jre
      mono

      # Encryption
      openssl
      gnupg

      # Develompent
      nix-repl
      nix-prefetch-scripts
      git
      subversion

      # Networking
      inetutils
      cifs_utils
      nfsUtils
      openvpn
      wget
      miniupnpc
      elinks

      # Utilities
      screen
      parallel
    ]) ++ (with config.boot.kernelPackages; [
      perf
    ]);
  };

  # Enable OpenGL support.
  hardware = {
    opengl.driSupport32Bit = true;

    #pulseaudio.configFile = ./default.pa;

    cpu.intel.updateMicrocode = true;
  };

  # Zsh with proper path
  programs.zsh.enable = true;
}
