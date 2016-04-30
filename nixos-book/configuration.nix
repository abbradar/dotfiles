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

  nix = {
    package = pkgs.nixUnstable;
    nixPath = [ "nixpkgs=/home/shlomo/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];
    daemonNiceLevel = 10;
    daemonIONiceLevel = 4;
  };

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
      noto-fonts-cjk
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
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment = {
    systemPackages = mkMerge [
      # multilib ldd in path
      (mkBefore [ pkgs.glibc_multi pkgs.utillinuxCurses ])
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
        blender
        mcomix
        anki

        # Browsing and related
        firefox
        chromium
        liferea
        deluge
        remmina
        wget

        # Encryption
        easyrsa
        truecrypt

        # Messaging and related
        thunderbird
        gajim
        skype
        mumble
        bitcoin
        tdesktop

        # Runtimes
        icedtea_web
        qemu

        # Multimedia
        (deadbeef-with-plugins.override {
          plugins = [ deadbeef-mpris2-plugin ];
        })
        ffmpeg
        #avidemux
        imagemagick
        mpv
        bomi
        pavucontrol
        youtube-dl
        imgurbash2
        soundfont-fluid
        geeqie

        # CD/DVD
        brasero

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
        nox
        julia
        (emacsWithPackages (with emacsPackagesNg; [
          evil undo-tree powerline-evil key-chord linum-relative ace-jump-mode
          use-package projectile magit
          company company-quickhelp company-nixos-options company-ghc
          flycheck flycheck-pos-tip flycheck-haskell
          yasnippet
          nixos-options nix-sandbox
          haskell-mode structured-haskell-mode #ghc
          org hamlet-mode ruby
          # idris-mode
          auctex auctex-latexmk
          ess
          rust-mode
          python jedi
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
        xiccd
        xkb_switch
        xlockmore
        rxvt_unicode-with-plugins
        system-config-printer

        # TeX
        (texlive.combine {
          inherit (texlive)
            collection-basic
            collection-bibtexextra
            collection-binextra
            collection-context
            collection-formatsextra
            collection-fontutils
            collection-genericextra
            collection-genericrecommended
            collection-langcyrillic
            collection-langenglish
            collection-latex
            collection-latexextra
            collection-latexrecommended
            collection-mathextra
            collection-pictures
            collection-plainextra
            collection-pstricks
            collection-science
            collection-xetex;
        })
        biber
        taffybar

        # Games
        (steam.override {
          withPrimus = true;
        })
        steam-run
        (dwarf-fortress.override {
          enableDFHack = true;
          theme = dwarf-fortress-packages.cla-theme;
        })
        the-powder-toy
        dwarf-therapist
        wesnoth
        zsnes
        adom
        doomseeker
        zandronum-bin
        lgogdownloader

        # Utils
        glxinfo
        tmux
        powertop
        sshfsFuse
        libcgroup

        # 3D printing
        cura
        slic3r

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
        stack
        
        cabal2nix
        ghc-core
        stylish-haskell
        hlint
        threadscope
        pointfree
        yesod-bin
        hasktags
        stylish-haskell
        #ghc-mod

        Agda
        #idris
      ])];

      pathsToLink = [ "/share/soundfonts" ];
    };

    # List services that you want to enable:
    services = {
      # SSH (for the times when I want additional slave)
      openssh.enable = true;
      #teamviewer.enable = true;

      tlp.enable = true;
      thermald.enable = true;

      # Printing
      printing = {
        enable = true;
        gutenprint = true;
        extraConf = ''
          LogLevel debug
        '';
      };

      # DBus
      dbus.packages = with pkgs; [ gnome.GConf system-config-printer ];

      gpm = {
        enable = true;
        protocol = "imps2";
      };

      # PostgreSQL
      postgresql = {
        enable = true;
        package = pkgs.postgresql95;
      };

      # Time synchronization.
      chrony.enable = true;
      ntp.enable = false;

      # Avahi
      avahi = {
        enable = true;
        nssmdns = true;
      };

      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        displayManager.sddm.enable = true;
        windowManager.xmonad = {
          enable = true;
          extraPackages = self: with self; [ taffybar ];
          enableContribAndExtras = true;
        };
        desktopManager.xfce = {
          enable = true;
          noDesktop = true;
        };
      };

      # For mah eyes.
      redshift.enable = true;
      colord.enable = true;

      # UDev
      udev.packages = with pkgs; [ android-udev-rules libmtp ];

      #logmein-hamachi.enable = true;
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
          extraGroups = [ "wheel" "networkmanager" "adbusers" "cdrom" ];
          uid = 1000;
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
