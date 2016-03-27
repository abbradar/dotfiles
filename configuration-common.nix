{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nix = {
    #package = pkgs.nixUnstable;

    gc = {
      automatic = true;
      dates = "weekly";
    };

    # Use chrooted builds.
    useChroot = true;

    extraOptions = ''
      auto-optimise-store = true
    '';

    # Hydra as a binary cache
    binaryCaches = [ http://hydra.nixos.org/ ];
    binaryCachePublicKeys = [ "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs=" ];
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

    wine.release = "staging";

    bochs = {
      debugger = true;
      disasm = true;
      debuggerGui = true;
    };

    # Performance
    zathura.useMupdf = true;

    packageOverrides = self: with self; {
      pidgin-with-plugins = pidgin-with-plugins.override {
        plugins = [ pidginlatex pidginotr ];
      };
      gajim = gajim.override {
        extraPythonPackages = pkgs: [ pkgs.python-axolotl ];
      };
      deadbeef-with-plugins = deadbeef-with-plugins.override {
        plugins = [ deadbeef-mpris2-plugin ];
      };
      xmonad-with-packages = xmonad-with-packages.override {
        packages = pkgs: with pkgs; [ taffybar xmonad-contrib xmonad-extras ];
      };
      xfce = xfce // {
        thunar-with-plugins = xfce.thunar-with-plugins.override {
          plugins = [ xfce.thunar_archive_plugin ];
        };
      };
      sudo = sudo.override {
        withInsults = true;
      };
      mpv = mpv.override {
        vaapiSupport = true;
      };
    };
  };

  boot = {
    # Use the latest kernel version.
    kernelPackages =
      let self = pkgs.linuxPackages_latest;
      in self // {
        bumblebee = self.bumblebee.override {
          #disablePciE = true;
        };
      };

    # https://github.com/NixOS/nixpkgs/issues/4825
    # cleanTmpDir = true;

    loader = {
      timeout = 0;
      efi.canTouchEfiVariables = true;
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
  i18n.consoleKeyMap = "ruwin_cplk-UTF-8";

  fonts.fontconfig.cache32Bit = true;

  services = {
    xserver = {
      layout = "us,ru";
      xkbOptions = "eurosign:e,grp:caps_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";
      enableCtrlAltBackspace = true;
    };

    openssh = {
      passwordAuthentication = false;
    };
  };

  # Packages
  environment = {
    systemPackages = (with pkgs; [
      # Monitors
      smartmontools
      dmidecode
      lm_sensors
      htop
      iotop
      ftop
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
      python2Full
      python3 # inconsistent
      ruby
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
      openvpn
      aria2
      socat
      elinks
      mtr

      # Utilities
      screen
      parallel
    ]) ++ (with config.boot.kernelPackages; [
      #perf
    ]);
  };

  # Disable power management defaults.
  # They can cause problems and we use TLP anyway
  powerManagement.scsiLinkPolicy = null;
  powerManagement.cpuFreqGovernor = null;

  # Enable OpenGL support.
  hardware = {
    opengl = {
      driSupport32Bit = true;
      s3tcSupport = true;
    };

    pulseaudio = {
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      configFile = ./default.pa;
    };

    cpu.intel.updateMicrocode = true;
  };
}
