# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

with pkgs.lib;

{
  imports =
    [ ../configuration-common.nix
      ./personal-configuration.nix
    ];

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    # For containers
    nat = {
      enable = true;
      internalInterfaces = ["ve-+"];
      externalInterface = "eth0";
    };
    extraHosts = ''
      0.0.0.0 nw2master.bioware.com
      0.0.0.0 nwn2.master.gamespy.com
      0.0.0.0 peerchat.gamespy.com
    '';
  };

  nix.nixPath = [ "nixpkgs=/home/shlomo/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];

  fonts = {
    # enableFontDir = true;
    # enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts # Microsoft free fonts
      cm_unicode
      stix-otf
      dejavu_fonts
      source-code-pro
      noto-fonts
      #noto-fonts-cjk
      noto-fonts-emoji
    ];
    fontconfig = { 
      dpi = 120;
      defaultFonts.monospace = [ "Source Code Pro" ];
    };
  };

  boot.supportedFilesystems = [ "nfs" "ntfs" "exfat" ];
  boot.kernelModules = [ "tun" "virtio" ];

  i18n.consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-v20n.psf.gz";

  #uim.enable = true;

  nixpkgs.config = {
    # Build packages with pulseaudio support
    pulseaudio = true;

    steam.primus = true;

    haskellPackageOverrides = self: super:
      let lib = pkgs.haskell.lib;
      in {
        xmonad-contrib = lib.appendPatch super.xmonad-contrib /home/shlomo/xmonad-contrib/xmonad-contrib.patch;
        taffybar = lib.appendPatch super.taffybar /home/shlomo/taffybar/taffybar.patch;
      };
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment = {
    systemPackages = mkMerge [
      # multilib ldd in path
      (mkBefore [ pkgs.glibc_multi ])
      (with pkgs; [
        # Files
        dropbox
        gnome3.file-roller

        baobab

        # Input
        anthy

        # Runtimes
        wine
        samba # needed for wine
        winetricks

        # Documents
        libreoffice
        gimp
        zathura
        xsane
        inkscape
        yed
        mcomix
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
        gajim
        skype
        mumble
        bitcoin
        qtox

        # Runtimes
        icedtea_web
        qemu

        # Multimedia
        (deadbeef-with-plugins.override {
          plugins = [ deadbeef-mpris2-plugin ];
        })
        ffmpeg
        imagemagick
        mpv
        bomi
        pavucontrol
        youtube-dl
        imgurbash
        soundfont-fluid

        # Math
        (rWrapper.override {
          packages = with rPackages; [
            lintr
            data_table
            parallel
          ];
        })
        graphviz

        # Development
        binutils
        clang
        gdb
        darcs
        mercurial
        subversion
        androidenv.platformTools
        patchelf
        nixopsUnstable
        (emacsWithPackages (with pkgs.emacsPackagesNg; [
          # packages are outdated; use package.el directly instead
          racer
        ]))

        # Networking
        networkmanagerapplet
        wireshark-gtk
        nmap
        miniupnpc

        # GUI-related
        polkit_gnome
        blueman
        xsel
        xkb_switch
        xlockmore
        rxvt_unicode-with-plugins
        # TeX
        #(texlive.combine {
        #  inherit (texlive) scheme-basic xetex latexmk dvipng
        #  nag beamer
        #  unicode-math
        #  filehook csquotes biblatex biber logreq tabu varwidth
        #  placeins titlesec totcount multirow lm-math
        #  dehyph-exptl hyphen-ancientgreek;
        #})
        texLiveFull
        biber
        taffybar
        xmonad-with-packages

        # Games
        steam
        (dwarf_fortress.override {
          enableDFHack = true;
        })
        the-powder-toy
        dwarf-therapist
        wesnoth
        zsnes
        adom

        # Utils
        glxinfo
        tmux
        powertop

        # Ruby development
        bundler_HEAD
        bundix
      ])
      (with pkgs.xfce; [
        xfce4_xkb_plugin
        xfce4_systemload_plugin
      ])
      (with pkgs.haskellPackages; [
        ((ghcWithPackages (self: with self; [ transformers
                                              mtl
                                              lens
                                            ]
                          )).override {
          withLLVM = true;
        })
        cabal-install
        
        cabal2nix
        ghc-core
        stylish-haskell
        hlint
        threadscope
        pointfree
        yesod-bin
        hasktags
        stylish-haskell

        Agda
        idris
      ])];

      pathsToLink = [ "/share/soundfonts" ];
    };

    # List services that you want to enable:
    services = {
      # SSH (for the times when I want additional slave)
      openssh.enable = true;

      tlp.enable = true;
      thermald.enable = true;

      #udev.extraRules =
      #  let rule = level: x: ''ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}==${toString level}, RUN+="${cmd x}"'';
      #      cmd = x: "${pkgs.dbus_tools}/bin/dbus-send --system --dest=org.freedesktop.thermald /org/freedesktop/thermald org.freedesktop.thermald.SetCurrentPreference string:${x}";
      #  in ''
      #    ${rule 0 "ENERGY_CONSERVE"}
      #    ${rule 1 "PERFORMANCE"}
      #  '';

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
        autorun = false;
        displayManager.sddm.enable = true;
        #displayManager.slim.enable = true;
        desktopManager.xfce = {
          enable = true;
        };
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

    programs = {
      # Zsh with proper path
      zsh.enable = true;
      cdemu.enable = true;
    };

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      defaultUserShell = "/run/current-system/sw/bin/zsh";
      mutableUsers = false;

      extraGroups = {
        adbusers = {};
      };

      extraUsers = {
        root.passwordFile = "/root/.passwd";
        shlomo = rec {
          group = "users";
          extraGroups = [ "wheel" "networkmanager" "adbusers" "cdrom" ];
          uid = 1000;
          home = "/home/shlomo";
          isNormalUser = true;
          passwordFile = "/root/.shlomo.passwd";
        };
        guest = rec {
          group = "users";
          uid = 2000;
          home = "/run/user/${toString uid}";
          isNormalUser = true;
          password = "123";
        };
      };
    };

  }
