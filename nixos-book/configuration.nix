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
      # Files
      dropbox

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

      # Encryption
      truecrypt

      # Messaging and related
      thunderbird
      #(pidginWrapper.override {
      #  plugins = [ pidginotr ];
      #})
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
      agda

      # Network
      networkmanagerapplet

      # GUI-related
      arandr
      xkb_switch
      xfontsel
      libnotify
      xlockmore
      #(rxvt_unicode_wrapper.override {
      #  plugins = [ urxvt_perls urxvt_tabbedex ];
      #})
      gnome.GConf

      # TeX
      texLiveFull

      # Games
      steamChrootEnv
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
      layout = "us,ru";
      xkbOptions = "eurosign:e,grp:caps_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";

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
        extraGroups = [ "wheel" "networkmanager" ];
        uid = 1000;
        home = "/home/shlomo";
        createHome = true;
        useDefaultShell = true;
      };
    };
  };

}
