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

        # Editors
        emacs
        gimp
      
        # Documents
        libreoffice
        gutenprint
        zathura
        xsane

        # Browsing and related
        firefoxWrapper
        chromium
        liferea
        deluge

        # Encryption
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

        # Network
        networkmanagerapplet

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

        # TeX
        texLiveFull

        # Games
        steamChrootEnv
        dwarf_fortress_2014
      ]) ++ (with pkgs.xfce; [
        xfce4_xkb_plugin
        xfce4_systemload_plugin
      ]) ++ (with pkgs.haskellPackages; [
        ghc
        cabalInstall
        cabal2nix
        happy
        alex
        c2hs
        ghcMod
        #threadscope

        criterion

        yesodBin

        # XMonad
        xmonad
        xmonadContrib
        xmonadExtras
        dbus

        Agda

        yiCustom
      ]);
    };

    # List services that you want to enable:
    services = {
      # Printing
      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint ];
      };

      # DBus
      dbus.packages = [ pkgs.gnome.GConf ];

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
            enableContribAndExtras = true;
            extraPackages = self: [ self.xmonadContrib self.xmonadExtras ];
          };
        };
        desktopManager.xfce.enable = true;
      };

      # UDev
      udev.packages = [ pkgs.libmtp ];
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
        root.passwordFile = "/root/.password";
        shlomo = rec {
          name = "shlomo";
          group = "users";
          extraGroups = [ "wheel" "networkmanager" ];
          uid = 1000;
          home = "/home/shlomo";
          createHome = true;
          useDefaultShell = true;
          passwordFile = "${home}/.password";
        };
      };
    };

  }
