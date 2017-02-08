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
      # Faster loading for NWN2
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
    binaryCaches = [ "http://cache.nixos.org" ];
  };

  fonts = {
    # enableFontDir = true;
    # enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts # Microsoft free fonts
      cm_unicode
      xits-math
      dejavu_fonts
      source-code-pro
      noto-fonts
      noto-fonts-cjk
      emojione
    ];
    fontconfig = { 
      dpi = 96;
      defaultFonts.monospace = [ "Source Code Pro" ];
    };
  };

  boot = {
    supportedFilesystems = [ "nfs" "ntfs" "exfat" ];
    kernelModules = [ "tun" "virtio" ];
    plymouth.enable = true;
    earlyVconsoleSetup = true;

  };

  i18n = {
    consoleFont = "ter-v20n";
    consolePackages = [ pkgs.terminus_font ];
    inputMethod.enabled = "ibus";
    inputMethod.ibus.engines = with pkgs.ibus-engines; [ uniemoji ];
  };

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
      # Big packages which we want to disable when experimenting.
      (let a = (with pkgs; [
        # Runtimes
        samba # needed for wine
        mono
        jre
        wine
        winetricks

        # Big suites
        chromium
        #firefox
        libreoffice
        tdesktop
        gimp
        inkscape
        blender
        (texlive.combine {
          inherit (texlive)
            collection-basic
            metafont
            xits
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

        # Games
        (steam.override {
          withPrimus = true;
        })
        (steam.override {
          withPrimus = true;
          nativeOnly = true;
          newStdcpp = true;
        }).run
        (dwarf-fortress.override {
          # enableDFHack = true;
          theme = dwarf-fortress-packages.cla-theme;
        })
        #dwarf-therapist
        wesnoth
        zeroad
        zsnes
        lgogdownloader
        dosbox

        # 3D printing
        cura
        slic3r
      ]) ++ (with pkgs.haskellPackages; [
          Agda
          #idris
      ]); in a)
      (with pkgs; [
        # Style
        terminus_font

        # Files
        btrfs-progs
        gnome3.file-roller
        baobab

        # Input
        anthy

        # Documents
        zathura
        xsane
        mcomix
        anki

        # Browsing and related
        qutebrowser
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
        pavucontrol
        youtube-dl
        imgurbash2
        soundfont-fluid
        geeqie
        simplescreenrecorder

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
        #julia
        (emacsWithPackages (with emacsPackagesNg; [
          evil undo-tree powerline-evil key-chord linum-relative ace-jump-mode
          use-package projectile magit
          company company-quickhelp company-nixos-options company-jedi
          flycheck flycheck-pos-tip flycheck-haskell
          yasnippet
          nixos-options nix-sandbox
          haskell-mode intero
          org hamlet-mode ruby
          # idris-mode
          auctex auctex-latexmk
          ess
          rust-mode
          python-mode cython-mode
        ]))

        # Qt development
        qtcreator
        (qt5.env "qt-${qt5.qtbase.version}" (with qt5; [ qtdeclarative ]))
        gnumake

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
        gksu
        (xmonad-with-packages.override {
          packages = self: with self; [ taffybar xmonad-contrib xmonad-extras ];
        })
        compton
        hsetroot

        # TeX
        biber
        taffybar

        # Utils
        glxinfo
        tmux
        powertop
        sshfsFuse
        libcgroup
        efibootmgr

        # Ruby development
        bundler_HEAD
        bundix

        # Doom
        zandronum-bin
        doomseeker
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
        stylish-haskell
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
      };

      # DBus
      dbus.packages = with pkgs; [ gnome2.GConf system-config-printer ];

      gnome3.gnome-keyring.enable = true;

      gpm = {
        enable = true;
        protocol = "imps2";
      };

      # PostgreSQL
      postgresql = {
        enable = true;
        package = pkgs.postgresql95;
      };

      # Avahi
      avahi = {
        enable = true;
        nssmdns = true;
      };

      # Enable the X11 windowing system.
      xserver = {
        enable = true;
        displayManager = {
          sddm.enable = true;
        };

        desktopManager = {
          default = "xfce";
          xterm.enable = false;
          xfce = {
            enable = true;
            noDesktop = true;
          };
        };
    
        windowManager = {
          default = "none";
        };
      };

      # For mah eyes.
      #redshift.enable = true;
      colord.enable = true;

      # UDev
      udev.packages = with pkgs; [ android-udev-rules libmtp ];

      # Disable lid switch handling
      logind.extraConfig = ''
        HandleLidSwitch=ignore
      '';

      # Proprietary services
      #logmein-hamachi.enable = true;
      #teamviewer.enable = true;
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

    boot.loader.timeout = 0;

    virtualisation = {
      virtualbox.host.enable = true;
      docker.enable = true;
    };

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      defaultUserShell = pkgs.zsh;
      mutableUsers = false;

      extraGroups = {
        adbusers = {};
      };

      extraUsers = {
        root.passwordFile = "/root/.passwd";

        shlomo = rec {
          extraGroups = [ "wheel" "networkmanager" "adbusers" "cdrom" "vboxusers" "docker" ];
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
