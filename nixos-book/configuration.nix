# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ ../configuration-common.nix
      ./personal-configuration.nix
    ];

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
  };

  boot.supportedFilesystems = [ "nfs" ];

  i18n.consoleFont = "ter-v16n";

  uim.enable = true;

  nixpkgs.config = {
    # Build packages with pulseaudio support
    pulseaudio = true;

    steam.primus = true;
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment = {
    sessionVariables.NIX_PATH = [ "nixthings=/home/shlomo/nixthings" ];
    systemPackages =
      (with pkgs; [
        # Files
        dropbox
        libmtp
        gparted
        #xfce.thunar_archive_plugin
        xarchiver

        # Input
        anthy

        # Runtimes
        wineUnstable
        winetricks

        # Documents
        libreoffice
        imagemagick
        gimp
        gutenprint
        zathura
        xsane
        (kde4.wrapper kde4.okular) # for commenting
        inkscape
        #yed
        mcomix
        xpdf
        anki

        # Browsing and related
        firefoxWrapper
        chromium
        liferea
        deluge
        remmina

        # Encryption
        easyrsa
        truecrypt

        # Messaging and related
        thunderbird
        pidgin-with-plugins
        skype
        mumble
        bitcoin

        # Runtimes
        icedtea7_web

        # Multimedia
        deadbeef
        bomi
        pavucontrol

        # Math
        (rWrapper.override {
          packages = with rPackages; [
            data_table
            parallel
          ];
        })
        graphviz

        # Development
        binutils
        gcc
        darcs
        mercurial
        androidsdk_4_4
        patchelf

        # Network
        networkmanagerapplet
        wireshark-gtk

        # GUI-related
        gnome3.gnome_themes_standard
        blueman
        xsel
        arandr
        xkb_switch
        xfontsel
        libnotify
        xlockmore
        gnome.GConf
        rxvt_unicode_with-plugins
        xmonad_log_applet_xfce
        glxinfo

        # TeX
        texLiveFull
        biber

        # Games
        glxinfo
        steam
        dwarf_fortress
        dwarf-therapist
        zsnes

        # Utils
        powertop

        # Ruby development
        bundler
        bundix
      ]) ++ (with pkgs.xfce; [
        xfce4_xkb_plugin
        xfce4_systemload_plugin
      ]) ++ (with pkgs.haskellngPackages; [
        ((ghcWithPackages (self: [])).override {
          withLLVM = true;
        })
        cabal2nix
        cabal-install
        hlint
        ghc-mod
        threadscope
        yesod-bin
        Agda
        idris
        hasktags
        stylish-haskell
        #yiCustom
        hasktags
      ]) ++ (with pkgs.emacsPackagesNg; [
        emacs
        
        evil
        auto-complete
        auctex
        ghc-mod
        structured-haskell-mode
        agda2-mode
        haskell-mode
        idris-mode
      ]);
    };

    # List services that you want to enable:
    services = {
      # SSH (for the times when I want additional slave)
      openssh.enable = true;

      #kmscon = {
      #  enable = true;
      #  hwRender = true;
      #};

      tlp.enable = true;
      thermald.enable = true;

      udev.extraRules =
        let rule = level: x: ''ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}==${toString level}, RUN+="${cmd x}"'';
            cmd = x: "${pkgs.dbus_tools}/bin/dbus-send --system --dest=org.freedesktop.thermald /org/freedesktop/thermald org.freedesktop.thermald.SetCurrentPreference string:${x}";
        in ''
          ${rule 0 "ENERGY_CONSERVE"}
          ${rule 1 "PERFORMANCE"}
        '';

      # Printing
      printing = {
        enable = true;
        drivers = with pkgs; [ gutenprint ];
      };

      # DBus
      dbus.packages = with pkgs; [ gnome.GConf ];

      gpm = {
        enable = true;
        protocol = "imps2";
      };

      # PostgreSQL
      postgresql = {
        enable = true;
        package = pkgs.postgresql93;
      };

      # Time synchronization.
      chrony.enable = true;
      ntp.enable = false;

      # Avahi
      avahi.enable = true;

      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        tty = 1;

        displayManager.sddm.enable = true;
        windowManager = {
          default = "xmonad";
          xmonad = {
            enable = true;
            extraPackages = self: with self; [ dbus ];
            enableContribAndExtras = true;
          };
        };
        desktopManager.xfce.enable = true;
      };

      # For mah eyes.
      redshift.enable = true;

      # UDev
      udev.packages = with pkgs; [ android-udev-rules libmtp ];
    };

    hardware = {
      # Enable PulseAudio.
      pulseaudio.enable = true;
      # Scanning
      sane.enable = true;
    };

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      defaultUserShell = "/var/run/current-system/sw/bin/zsh";
      mutableUsers = false;

      extraGroups = {
        adbusers = {};
      };

      extraUsers = {
        root.passwordFile = "/root/.passwd";
        shlomo = rec {
          group = "users";
          extraGroups = [ "wheel" "networkmanager" "adbusers" ];
          uid = 1000;
          home = "/home/shlomo";
          createHome = true;
          useDefaultShell = true;
          #passwordFile = "${home}/.password";
          passwordFile = "/root/.shlomo.passwd";
        };
      };
    };

  }
