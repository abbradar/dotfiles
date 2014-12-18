# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ ../configuration-common.nix
    ];

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
  };

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
        yed
        mcomix
        xpdf

        # Browsing and related
        firefoxWrapper
        chromium
        liferea
        deluge

        # Encryption
        easyrsa
        truecrypt

        # Messaging and related
        thunderbird
        pidgin-with-plugins
        skype
        mumble
        bitcoin

        # Multimedia
        deadbeef
        cmplayer
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
      ]) ++ (with pkgs.xfce; [
        xfce4_xkb_plugin
        xfce4_systemload_plugin
      ]) ++ (with pkgs.haskellPackages; [
        ghc
        cabal2nix
        cabalInstall
        hlint
        ghcMod
        threadscope
        yesodBin
        Agda
        yiCustom
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

      # Printing
      printing = {
        enable = true;
        drivers = with pkgs; [ gutenprint ];
      };

      # DBus
      dbus.packages = with pkgs; [ gnome.GConf ];

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
            extraPackages = self: with self; [ xmonadContrib xmonadExtras h-booru dbus ];
          };
        };
        desktopManager.xfce.enable = true;
      };

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
