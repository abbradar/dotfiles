{ lib, config, pkgs, ... }:

with lib;

let
  myPass = pkgs.pass.withExtensions (exts: with exts; [ pass-otp ]);

  /*jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    # Example working revision, check out the latest one.
    rev = "2b10030df2a29beed10c02d5f64745b143206350";
  }) {};

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy pandas ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython ];
    };*/

in {
  imports =
    [ ./configuration-common.nix
    ];

  fonts.packages = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    source-code-pro
    cm_unicode
    inter
    corefonts
    nerdfonts
  ];

  nix.nixPath = [ "nixpkgs=/home/abbradar/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];

  # Time zone
  time.timeZone = "Asia/Bangkok";

  nixpkgs.config = {
    android_sdk.accept_license = true;
    # Build packages with pulseaudio support
    pulseaudio = true;
  };

  boot = {
    loader.timeout = 0;
    supportedFilesystems = [ "nfs" "ntfs" "exfat" ];
    kernelModules = [ "v4l2loopback" ];
    # kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback
    ];
    plymouth.enable = true;
    kernel.sysctl."kernel.sysrq" = 1;
  };

  networking = {
    firewall.enable = false;
    wireguard.enable = true;
    networkmanager = {
      enable = true;
      ethernet.macAddress = "stable";
      wifi.macAddress = "stable";
    };
    # Needed for nasty captive portals to work.
    resolvconf.dnsExtensionMechanism = false;
  };

  environment.pathsToLink = [ "/libexec" ];

  environment.systemPackages = with pkgs; [
    # Utils
    powertop
    s-tui
    # minicom
    mercurial
    google-cloud-sdk
    pavucontrol
    androidenv.androidPkgs.platform-tools
    #platformio
    silver-searcher
    myPass
    git-lfs
    git-filter-repo

    # Runtimes
    steam-run-native
    (appimage-run.override {
      extraPkgs = pkgs: [ pkgs.icu ];
    })
    steam
    lgogdownloader
    wineWowPackages.staging
    lutris
    zenity
    jdk
    leiningen
    nodejs
    # icedtea_web
    adoptopenjdk-icedtea-web

    # VM
    virt-manager
    docker-compose

    # Multimedia
    (firefox.override {
      nativeMessagingHosts = [ tridactyl-native gnome-browser-connector (passff-host.override { pass = myPass; }) ];
    })
    chromium
    (deadbeef-with-plugins.override { 
      plugins = with deadbeefPlugins; [ mpris2 ]; 
    })
    thunderbird
    (mpv.override {
      scripts = with mpvScripts; [ inhibit-gnome ];
    })
    yt-dlp
    stremio
    syncplay
    gimp
    # Broken
    #darktable
    audacity
    inkscape
    xsane
    # zathura
    obs-studio
    qjackctl
    pulseaudio # for pacmd

    # GUI
    wl-clipboard
    gnome-tweaks
    gnome-themes-extra

    # Messengers
    gajim
    dino
    element-desktop
    # Has issues with Wayland
    tdesktop
    /*(writers.writeBashBin "telegram-desktop" ''
      if [ -z "''${QT_QPA_PLATFORM+x}" ]; then
        export QT_QPA_PLATFORM=xcb
      fi
      exec ${tdesktop}/bin/telegram-desktop "$@"
    '')*/
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
    emacs
    # emacs-unstable
    # neovim-qt
    # For doom
    ripgrep fd direnv fzf
    # rtags
    glslang
    llvmPackages_latest.clang
    clang-tools
    pyright
    beamPackages.elixir-ls
    shellcheck

    # Network
    deluge
    miniupnpc
    wget
    openvpn
    update-resolv-conf
    remmina
    shadowsocks-libev
    tor

    # Documents
    libreoffice
    anki
    nextcloud-client
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
    python3.pkgs.pygments
  ];

  environment.gnome.excludePackages = with pkgs; [gnome3.gnome-software];

  i18n = {
    inputMethod.enabled = "ibus";
  };

  console = {
    packages = [ pkgs.terminus_font ];
  };

  sound.enable = true;

  hardware = {
    # We use PipeWire
    pulseaudio.enable = false;
    sane = {
      enable = true;
    };
    xpadneo.enable = true;
    steam-hardware.enable = true;
    usb-modeswitch.enable = true;
  };

  documentation.nixos.enable = false;

  services = {
    # k3s.enable = true;
    # teamviewer.enable = true;
    pipewire.enable = true;
    flatpak.enable = true;
    system-config-printer.enable = false;
    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
    };
    pcscd = {
      enable = true;
    };

    resolved.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
    };
    printing = {
      enable = true;
      drivers = with pkgs; [ epson-escpr gutenprint ];
    };

    dnscrypt-proxy2 = {
      # enable = true;
      settings = {
        server_names = [ "cloudflare" ];
        force_tcp = true;
        log_level = 0;
      };
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
       ];
      };
    };

    gnome.rygel.enable = true;

    samba = {
      # enable = true;
      extraConfig = ''
        bind interfaces only = yes
        interfaces = virbr0
        acl allow execute always = yes
      '';
      shares.home = {
        path = "/home/abbradar";
        "browseable" = "yes";
        "read only" = "no";
      };
    };
  };

  programs = {
    zsh.enable = true;
    cdemu.enable = true;
    gnome-terminal.enable = true;
  };

  users = {
    mutableUsers = false;
    defaultUserShell = pkgs.zsh;

    extraUsers = {
      root.hashedPasswordFile = "/root/.passwd";
      abbradar = {
        hashedPasswordFile = "/root/.abbradar.passwd";
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "docker" "podman" "wireshark" "libvirtd" "cdrom" "vboxusers" ];
      };
    };
  };

  programs = {
    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };

    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    neovim = {
      enable = true;
      withPython3 = true;
      configure = {
        packages.myVimPackage = with pkgs.vimPlugins; {
          start = [
            (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
            coq_nvim
          ];
        };
      };
    };
  };

  security.rtkit.enable = true;

  security.pam.loginLimits = [
    { domain = "abbradar";
      type = "-";
      item = "memlock";
      value = "unlimited";
    }
    { domain = "abbradar";
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
