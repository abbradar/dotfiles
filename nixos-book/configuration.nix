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

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts  # Microsoft free fonts
      cm_unicode
      dejavu_fonts
      ttf_bitstream_vera
      ipafont
    ];
  };

  nixpkgs.config = {
    # Build packages with pulseaudio support
    pulseaudio = true;
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment = {
    sessionVariables.NIX_PATH = [ "nixthings=/home/shlomo/nixthings" ];
    pathsToLink = [ "/etc/gconf" ];
    systemPackages =
      (with pkgs; [
        # Files
        dropbox
        libmtp
        gparted
        #xfce.thunar_archive_plugin
        xarchiver

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
        rWrapper
        graphviz

        # Development
        llvm
        binutils
        gcc
        darcs
        mercurial

        # Network
        networkmanagerapplet
        wireshark-gtk

        # GUI-related
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
        steamChrootEnv
        dwarf_fortress
        dwarf-therapist
        zsnes

        # Utils
        powertop
      ]) ++ (with pkgs.xfce; [
        xfce4_xkb_plugin
        xfce4_systemload_plugin
      ]) ++ (with pkgs.haskellngPackages; [
        ghc
        cabal2nix
        cabal-install
        hlint
        ghc-mod
        threadscope
        yesod-bin
        Agda
        hasktags
        stylish-haskell
      ]) ++ (with pkgs.emacs24Packages; [
        autoComplete
        emacs
        #cedet
        haskellMode
        org
        structuredHaskellMode
        ess
      ]);
    };

    # List services that you want to enable:
    services = {
      # SSH (for the times when I want additional slave)
      openssh.enable = true;

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
        #tty = 1;

        displayManager.lightdm.enable = true;
        windowManager = {
          default = "xmonad";
          xmonad = {
            enable = true;
            extraPackages = self: with self; [ dbus ]; # https://github.com/Fuuzetsu/h-booru/issues/2
            enableContribAndExtras = true;
          };
        };
        desktopManager.xfce.enable = true;
      };

      # For mah eyes.
      redshift.enable = true;

      # UDev
      udev.packages = with pkgs; [ libmtp ];
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

      extraUsers = {
        root.passwordFile = "/root/.passwd";
        shlomo = rec {
          group = "users";
          extraGroups = [ "wheel" "networkmanager" ];
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
