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
      pidgin-with-plugins = pidgin-with-plugins.override {
        plugins = [ pidginlatex pidginotr ];
      };
      deadbeef-with-plugins = deadbeef-with-plugins.override {
        plugins = [ deadbeef-mpris2-plugin ];
      };
      xfce = xfce // {
        thunar-with-plugins = xfce.thunar-with-plugins.override {
          plugins = [ xfce.thunar_archive_plugin ];
        };
      };
      sudo = sudo.override {
        withInsults = true;
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
    sudo.extraConfig = ''
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
  };

  # Packages
  environment = {
    systemPackages = (with pkgs; [
      # Management
      smartmontools
      dmidecode
      lm_sensors
      htop
      iotop
      nethogs
      psmisc
      lsof
      pciutils
      usbutils

      # Partitions
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
      pastebinit

      # Runtimes
      python3
      ruby_2_2
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
      nfs-utils
      openvpn
      wget
      socat
      miniupnpc
      elinks

      # Utilities
      screen
      parallel
    ]) ++ (with config.boot.kernelPackages; [
      #perf
    ]);
  };

  # Enable OpenGL support.
  hardware = {
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };

    pulseaudio = {
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      #configFile = ./default.pa;
    };

    cpu.intel.updateMicrocode = true;
  };

  # Zsh with proper path
  programs.zsh.enable = true;
}
