# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

#let common = import ../configuration-common.nix { inherit config pkgs; }; in
{
  imports =
    [ # Include the results of the hardware scan.
      #/etc/nixos/hardware-configuration.nix
      ../configuration-common.nix
    ];

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "ter-v16n";
    consoleKeyMap = "ruwin_cplk-UTF-8";
  };

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

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment = {
    pathsToLink = [ "/etc/gconf" ];
    systemPackages = (with pkgs; [
      # Appearance
      oxygen-gtk2
      oxygen-gtk3

      # Files
      dropbox
      truecrypt

      # Editors
      emacs

      # Browsing and related
      firefoxWrapper
      chromium
      liferea
      deluge

      # Viewers
      libreoffice
      zathura

      # Messaging and related
      thunderbird
      pidgin
      pidginotr
      #gajim
      skype
      gnupg
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

      # Utilities
      xkb_switch

      # GUI
      gnome.GConf

      # TeX
      texLiveFull

      # Games
      steamChrootEnv
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

      xmonad
      xmonadContrib
      xmonadExtras

      yesodBin
    ]) ++ (with pkgs.kde4; [
      networkmanagement
      kde_gtk_config
      kde_wacomtablet
      kmix
      kdegraphics
      kdeutils
      applications
      kactivities
      kdeadmin
      kdenetwork
      kdepim
      kdeplasma_addons
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
      tty = 1;
      layout = "us,ru";
      xkbOptions = "eurosign:e,grp:caps_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";

      # Enable the KDE Desktop Environment.
      displayManager.kdm.enable = true;
      desktopManager.kde4.enable = true;
      windowManager = {
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
    };
  };

  hardware = {
    # Enable PulseAudio.
    pulseaudio.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    extraUsers = {
      shlomo = {
        name = "shlomo";
        group = "users";
        extraGroups = [ "wheel" "networkmanager" "audio" ];
        uid = 1000;
        home = "/home/shlomo";
        createHome = true;
        useDefaultShell = true;
      };
    };
  };

}
