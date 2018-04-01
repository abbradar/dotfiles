{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    daemonNiceLevel = 10;
    daemonIONiceLevel = 4;

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
      icedtea = true;
    };

    chromium = {
      enablePepperPDF = true;
    };

    bochs = {
      debugger = true;
      disasm = true;
      debuggerGui = true;
    };

    haskellPackageOverrides = self: super: with pkgs.haskell.lib; {
      xmonad-contrib = appendPatch super.xmonad-contrib (pkgs.fetchpatch {
        name = "net_wm_state_fullscreen.patch";
        url = "http://git.pld-linux.org/gitweb.cgi?p=packages/ghc-xmonad-contrib.git;a=blob_plain;f=net_wm_state_fullscreen.patch;h=499c76422424465cce488198e5295d0fba6f32ff;hb=904aa3f61cb4a5d2cd0dae7c1b2436ccf360b2df";
        extraPrefix = "";
        sha256 = "0s08k20403796hw4rhpvvzipy8a773qpym07jcnl8gla0p7qjzrj";
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
      mumble_git = mumble_git.override {
        speechdSupport = true;
        speechd = speechd.override {
          withEspeak = true;
        };
      };
      wine = wineStaging;
    };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    cleanTmpDir = true;

    loader.efi.canTouchEfiVariables = true;
  };

  # https://github.com/NixOS/nixpkgs/issues/4825
  systemd.services = {
    systemd-tmpfiles-setup.before = [ "sysinit.target" ];
    systemd-update-utmp.after = [ "systemd-tmpfiles-setup.service" ];
  };

  # Time zone
  time.timeZone = "Europe/Moscow";

  # Security
  security = {
    sudo.extraConfig = ''
      Defaults rootpw,insults,timestamp_timeout=60
    '';
    pam.loginLimits = [
      { domain = "*"; item = "nofile"; type = "-"; value = "4096"; }
    ];
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

    udev.extraHwdb = ''
      # Wheel key on Microsoft Natural Ergonomic Keyboard 4000/7000
      evdev:input:b0003v045Ep00DB*
        KEYBOARD_KEY_c022d=pageup
        KEYBOARD_KEY_c022e=pagedown

      evdev:input:b0003v045Ep071D*
        KEYBOARD_KEY_c022d=pageup
        KEYBOARD_KEY_c022e=pagedown
    '';
  };

  # Packages
  environment = {
    systemPackages = (with pkgs; [
      # Monitors
      htop
      iotop
      ftop
      nethogs
      psmisc
      lsof

      # Hardware
      dmidecode
      lm_sensors
      pciutils
      usbutils
      hdparm
      ethtool
      smartmontools

      # Files
      gptfdisk
      p7zip
      zip
      unzip
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

      # Encryption
      openssl
      gnupg

      # Develompent
      git
      subversion

      # Networking
      inetutils
      dnsutils
      aria2
      socat
      mtr

      # Utilities
      screen
      parallel
      mkpasswd

      rxvt_unicode.terminfo
    ]) ++ (with config.boot.kernelPackages; [
      #perf
    ]);
  };

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
  };
}
