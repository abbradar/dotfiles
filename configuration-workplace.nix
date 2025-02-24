{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  myPass = pkgs.pass.withExtensions (exts: with exts; [pass-otp]);
in {
  imports = [
    ./configuration-common.nix
  ];

  fonts.packages = with pkgs;
    [
      dejavu_fonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      source-code-pro
      cm_unicode
      inter
      corefonts
    ]
    ++ filter isDerivation (attrValues nerd-fonts);

  nix.nixPath = ["nixpkgs=/home/abbradar/nixpkgs" "nixos-config=/etc/nixos/configuration.nix"];

  # Time zone
  time.timeZone = "Asia/Bangkok";

  nixpkgs.config = {
    android_sdk.accept_license = true;
    # Build packages with pulseaudio support
    pulseaudio = true;
  };

  stylix = {
    enable = true;
    polarity = "dark";
    image = ./wallpaper.png;
  };

  boot = {
    loader.timeout = 0;
    supportedFilesystems = ["nfs" "ntfs" "exfat"];
    kernelModules = ["v4l2loopback"];
    # kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback
    ];
    plymouth.enable = true;
    kernel.sysctl."kernel.sysrq" = 1;
  };

  networking = {
    firewall.enable = false;
    wireguard.enable = false;
    networkmanager = {
      enable = true;
      ethernet.macAddress = "stable";
      wifi.macAddress = "stable";
    };
    # Needed for nasty captive portals to work.
    resolvconf.dnsExtensionMechanism = false;
  };

  environment.pathsToLink = ["/libexec"];

  environment.systemPackages = with pkgs; [
    # Utils
    powertop
    s-tui
    # minicom
    pavucontrol
    androidenv.androidPkgs.platform-tools
    #platformio
    myPass
    git-lfs
    git-filter-repo
    rclone
    pwgen

    # Runtimes
    steam-run-native
    (appimage-run.override {
      extraPkgs = pkgs: [pkgs.icu];
    })
    steam
    wineWowPackages.staging
    lutris
    jdk
    leiningen
    nodejs
    adoptopenjdk-icedtea-web

    # VM
    virt-manager
    kubectl

    # Multimedia
    (firefox.override {
      nativeMessagingHosts = [tridactyl-native gnome-browser-connector (passff-host.override {pass = myPass;})];
    })
    chromium
    (deadbeef-with-plugins.override {
      plugins = with deadbeefPlugins; [mpris2];
    })
    thunderbird
    (mpv.override {
      scripts = with mpvScripts; [inhibit-gnome];
    })
    yt-dlp
    stremio
    syncplay
    gimp
    audacity
    inkscape
    obs-studio
    qjackctl
    # Broken
    #darktable
    #xsane

    # GUI
    wl-clipboard
    gnome-tweaks
    gnome-themes-extra

    # Messengers
    gajim
    element-desktop
    tdesktop
    signal-desktop
    mumble
    zoom-us

    # Haskell
    irony-server
    cabal-install
    haskellPackages.haskell-language-server
    #jupyterEnvironment
    ghc
    # stack
    cabal2nix

    # Development
    vscode
    code-cursor
    obsidian
    emacs
    # For doom
    ripgrep
    fd
    direnv
    fzf
    # rtags
    llvmPackages_latest.clang
    clang-tools
    shellcheck
    aichat
    clickhouse

    # Network
    deluge
    miniupnpc
    wget
    update-resolv-conf
    remmina
    shadowsocks-libev
    tor

    # Documents
    libreoffice
    anki
    nextcloud-client
    (texlive.combine {
      inherit
        (texlive)
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
        collection-xetex
        ;
    })
    python3.pkgs.pygments
  ];

  environment.gnome.excludePackages = with pkgs; [gnome-software];

  i18n = {
    inputMethod = {
      enable = true;
      type = "ibus";
    };
  };

  console = {
    packages = [pkgs.terminus_font];
  };

  hardware = {
    sane = {
      enable = true;
    };
    xpadneo.enable = true;
    steam-hardware.enable = true;
    usb-modeswitch.enable = true;
    flipperzero.enable = true;
  };

  documentation.nixos.enable = false;

  services = {
    # k3s.enable = true;
    # teamviewer.enable = true;
    # We use PipeWire
    pulseaudio.enable = false;
    pipewire.enable = true;
    flatpak.enable = true;
    system-config-printer.enable = true;
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
    };

    resolved.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
    };
    printing = {
      enable = true;
      drivers = with pkgs; [epson-escpr gutenprint];
    };

    udev.packages = with pkgs; [
      android-udev-rules
      libmtp
    ];

    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      wacom.enable = true;
      desktopManager.gnome = {
        enable = true;
        sessionPath = with pkgs.gnomeExtensions; [
          caffeine
          appindicator
          paperwm
        ];
      };
    };

    gnome.rygel.enable = true;

    samba = {
      # enable = true;
      settings.global = {
        "bind interfaces only" = true;
        "interfaces" = "virbr0";
        "acl allow execute always" = true;
      };
      settings.home = {
        path = "/home/abbradar";
        "browseable" = "yes";
        "read only" = "no";
      };
    };
  };

  users = {
    extraUsers = {
      root.hashedPasswordFile = "/root/.passwd";
      abbradar = {
        hashedPasswordFile = "/root/.abbradar.passwd";
        isNormalUser = true;
        uid = 1000;
        extraGroups = ["wheel" "docker" "podman" "wireshark" "libvirtd" "cdrom" "vboxusers"];
      };
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    users.abbradar = ./home.nix;
  };

  programs = {
    cdemu.enable = true;
    gnome-terminal.enable = true;
    direnv.enable = true;

    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };

    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };
  };

  security.rtkit.enable = true;

  security.pam.loginLimits = [
    {
      domain = "abbradar";
      type = "-";
      item = "memlock";
      value = "unlimited";
    }
    {
      domain = "abbradar";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];

  virtualisation = {
    docker = {
      enable = true;
      # Fix working with networkd.
      # enableSysbox = true;
    };
    # podman = {
    #   enable = true;
    #   dockerSocket.enable = true;
    # };
    libvirtd.enable = true;
    virtualbox.host = {
      enable = true;
      # enableExtensionPack = true;
    };
  };
}
