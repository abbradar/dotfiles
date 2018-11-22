{ config, pkgs, ... }:

with pkgs.lib;

{
  imports =
    [ ./configuration-common.nix
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

  fonts = {
    fonts = with pkgs; [
      corefonts # Microsoft free fonts
      cm_unicode
      xits-math
      dejavu_fonts
      source-code-pro
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
    ];
    fontconfig = { 
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
        #libreoffice
        tdesktop
        signal-desktop
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
            #collection-genericextra
            #collection-genericrecommended
            collection-langcyrillic
            collection-langenglish
            collection-latex
            collection-latexextra
            collection-latexrecommended
            #collection-mathextra
            collection-pictures
            #collection-plainextra
            collection-pstricks
            #collection-science
            collection-xetex;
        })

        # Games
        (steam.override {
          withPrimus = true;
        })
        (steam.override {
          withPrimus = true;
          nativeOnly = true;
        }).run
        #(dwarf-fortress.override {
        #  # enableDFHack = true;
        #  theme = dwarf-fortress-packages.cla-theme;
        #})
        #dwarf-therapist
        wesnoth
        #zeroad
        zsnes
        lgogdownloader
        dosbox
        #zandronum
        #doomseeker

        # 3D printing
        #cura
      ]) ++ (with pkgs.haskellPackages; [
          #Agda
          #idris
      ]); in a)
      (with pkgs; [
        # Style
        terminus_font
        gnome3.gnome_themes_standard

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
        firefox
        chromium
        deluge
        remmina
        wget
        liferea

        # Encryption
        easyrsa
        truecrypt
        tor

        # Messaging and related
        thunderbird
        gajim
        skypeforlinux
        mumble_git

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
        paprefs
        youtube-dl
        imgurbash2
        geeqie
        simplescreenrecorder

        # CD/DVD
        brasero

        # Math
        graphviz

        # Development
        binutils
        gcc # clang is buggy on NixOS right now
        gdb
        darcs
        mercurial
        minicom
        subversion
        androidenv.platformTools
        patchelf
        nox
        (emacsWithPackages (with emacsPackagesNg; [
          evil evil-nerd-commenter undo-tree powerline-evil key-chord linum-relative ace-jump-mode
          use-package projectile magit
          company company-quickhelp
          flycheck flycheck-pos-tip
          yasnippet
          nix-sandbox
          haskell-mode intero
          org
          auctex auctex-latexmk
          ess
          rust-mode
          python-mode
          cider clojure-mode elm-mode markdown-mode lua-mode fsharp-mode csharp-mode yaml-mode
        ]))

        # Qt development
        qtcreator
        (qt5.env "qtenv-${qt5.qtbase.version}" (with qt5; [ qtdeclarative qtquickcontrols qtquickcontrols2 ]))
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
        linuxPackages.cpupower
        sshfsFuse
        libcgroup
        efibootmgr

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
        hlint
        #threadscope
        pointfree
        #yesod-bin
      ])
    ];

    pathsToLink = [ "/share/soundfonts" ];
  };

  # List services that you want to enable:
  services = {
    postgresql = {
      package = pkgs.postgresql_10;
      enable = true;
      extraConfig = ''
        log_statement = all
      '';
    };

    # Printing
    printing = {
      enable = true;
      drivers = with pkgs; [ epson-escpr gutenprint ];
    };

    # DBus
    dbus.packages = with pkgs; [ gnome2.GConf system-config-printer ];

    gnome3.gnome-keyring.enable = true;

    gpm = {
      enable = true;
      protocol = "imps2";
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
    udev = {
      packages = with pkgs; [ android-udev-rules libmtp m33-linux ];
      extraRules = ''
        SUBSYSTEM=="usb", ATTRS{idVendor}=="10cf", ATTRS{idProduct}=="2501", GROUP="wheel", MODE="0660"
      '';
    };

    # Proprietary services
    # logmein-hamachi.enable = true;
  };

  hardware = {
    # Enable PulseAudio.
    pulseaudio.enable = true;
    # Scanning
    sane.enable = true;
  };

  # For Unity and others.
  security.chromiumSuidSandbox.enable = true;

  programs = {
    # Zsh with proper path
    zsh.enable = true;
    cdemu.enable = true;
  };

  boot.loader.timeout = 0;

  virtualisation = {
    virtualbox.host = {
      enable = true;
      enableHardening = false;
    };
    docker.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;

    extraGroups = {
      adbusers = {};
    };
  };
}
