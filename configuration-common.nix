{ lib, config, pkgs, ... }:

with lib;
{
  nix = {
    # package = pkgs.nixUnstable;
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 4;

    gc = {
      automatic = true;
      dates = "weekly";
    };

    settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes" "repl-flake"];
    };
  };

  nixpkgs.config = {
    # Allow unfree packages.
    allowUnfree = true;

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
      deadbeef-with-plugins = deadbeef-with-plugins.override {
        plugins = [ deadbeef-mpris2-plugin ];
      };
      wine = wineStaging;
    };
  };

  boot = {
    tmp.cleanOnBoot = true;

    loader.efi.canTouchEfiVariables = true;
  };

  # Security
  security = {
    sudo.extraConfig = ''
      Defaults rootpw,insults,timestamp_timeout=60
    '';
    pam.loginLimits = [
      { domain = "*"; item = "nofile"; type = "-"; value = "4096"; }
    ];
  };

  console = {
    # Select internationalization properties.
    keyMap = "ruwin_cplk-UTF-8";
  };

  fonts.fontconfig.cache32Bit = true;

  services = {
    xserver = {
      xkb = {
        layout = "us,ru";
        options = concatStringsSep "," [
          "eurosign:e"
          "grp:caps_toggle"
          "grp_led:scroll"
          "terminate:ctrl_alt_bksp"
          "compose:ralt"
        ];
      };
      enableCtrlAltBackspace = true;
    };

    openssh = {
      settings = {
        PasswordAuthentication = false;
      };
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

    pipewire = {
      jack.enable = true;
      pulse.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
  };

  systemd.user.services.pipewire-pulse = mkIf config.services.pipewire.enable {
    postStart = ''
      if (( UID >= 1000 )); then
        ${pkgs.pulseaudio}/bin/pactl load-module module-switch-on-connect || true
        ${pkgs.pulseaudio}/bin/pactl load-module module-zeroconf-discover || true
      fi
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
      zip
      unzip
      tree
      rsync
      file
      pv
      dos2unix

      # Editors
      pastebinit

      # Runtimes
      python3
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
      sshfs-fuse
      curlftpfs

      # Utilities
      screen
      parallel-full
      jq
      mkpasswd

      rxvt_unicode.terminfo
    ]) ++ (with config.boot.kernelPackages; [
      #perf
    ]);
  };

  programs = {
   neovim = {
     enable = true;
     viAlias = true;
     vimAlias = true;
   };

    ssh.extraConfig = ''
      ServerAliveInterval 60
    '';
  };

  hardware = {
    graphics.enable32Bit = true;

    bluetooth = {
       # package = pkgs.bluez5-experimental;
       settings = {
         General = {
           # ControllerMode = "bredr";
           # ControllerMode = "le";
         };
       };
     };
  };
}
