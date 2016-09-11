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

    # Use sandboxed builds.
    useSandbox = true;

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

    wine.release = "staging";

    bochs = {
      debugger = true;
      disasm = true;
      debuggerGui = true;
    };

    haskellPackageOverrides = self: super: with pkgs.haskell.lib; {
      xmonad-contrib = appendPatch super.xmonad-contrib (pkgs.fetchpatch {
        name = "net_wm_state_fullscreen.patch";
        url = "http://git.pld-linux.org/gitweb.cgi?p=packages/ghc-xmonad-contrib.git;a=blob_plain;f=net_wm_state_fullscreen.patch;h=499c76422424465cce488198e5295d0fba6f32ff;hb=904aa3f61cb4a5d2cd0dae7c1b2436ccf360b2df";
        sha256 = "0s08k20403796hw4rhpvvzipy8a773qpym07jcnl8gla0p7qjzrj";
        addPrefixes = true;
      });
    };

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
      mumble = mumble.override {
        speechdSupport = true;
        speechd = speechd.override {
          withEspeak = true;
        };
      };
    };
  };

  boot = {
    # Use the latest kernel version.
    kernelPackages = pkgs.linuxPackages_latest;
      # NVIDIA doesn't support 4.6 for now
      # let self = pkgs.linuxPackages_latest;

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
      bind
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

  programs.ssh.extraConfig = ''
    ServerAliveInterval 60
  '';

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
