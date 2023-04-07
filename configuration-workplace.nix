{ lib, config, pkgs, ... }:

with lib;

let
  myUtsushi = pkgs.utsushi.override { withNetworkScan = true; };

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

  fonts.fonts = with pkgs; [
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

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/f22fe28d687085b9c969f84b8784aca67b7340f4.tar.gz;
    }))
  ];

  # Time zone
  time.timeZone = "Asia/Bangkok";

  nixpkgs.config = {
    android_sdk.accept_license = true;
    # Build packages with pulseaudio support
    pulseaudio = true;
    firefox = {
      enableBrowserpass = true;
      enableGnomeExtensions = true;
      enableTridactylNative = true;
    };
  };

  boot = {
    loader.timeout = 0;
    supportedFilesystems = [ "nfs" "ntfs" "exfat" ];
    kernelModules = [ "v4l2loopback" ];
    kernelPackages = pkgs.linuxPackages_latest;
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
    extraHosts = ''
      10.2.3.20 git.sib.team
      10.2.2.253 git.sib.team
    '';
  };

  environment.pathsToLink = [ "/libexec" ];

  environment.systemPackages = with pkgs; [
    # Utils
    powertop
    s-tui
    minicom
    mercurial
    google-cloud-sdk
    pavucontrol
    androidenv.androidPkgs_9_0.platform-tools
    #platformio
    silver-searcher
    myPass
    git-lfs
    jq

    # Runtimes
    steam-run-native
    (appimage-run.override {
      extraPkgs = pkgs: [ pkgs.icu ];
    })
    steam
    lgogdownloader
    wineWowPackages.staging
    lutris
    gnome3.zenity
    jdk
    leiningen
    icedtea_web

    # VM
    virtmanager
    docker-compose

    # Multimedia
    (firefox.override {
      extraNativeMessagingHosts = [ (passff-host.override { pass = myPass; }) ];
    })
    chromium
    (deadbeef-with-plugins.override { 
      plugins = with deadbeefPlugins; [ mpris2 ]; 
    })
    thunderbird
    mpv
    yt-dlp
    stremio
    syncplay
    gimp
    darktable
    audacity
    inkscape
    xsane
    # zathura
    obs-studio
    qjackctl
    pulseaudio # for pacmd

    # GUI
    wl-clipboard

    # Messengers
    gajim
    element-desktop
    tdesktop
    signal-desktop
    mumble_git
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
    emacsNativeComp
    # neovim-qt
    # For doom
    ripgrep fd direnv fzf
    # rtags
    glslang
    llvmPackages_latest.clang
    clang-tools
    pyright

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
      # extraBackends = [ myUtsushi ];
    };
    xpadneo.enable = true;
    steam-hardware.enable = true;
    usbWwan.enable = true;
  };

  documentation.nixos.enable = false;

  services = {
    #k3s.enable = true;
    # teamviewer.enable = true;
    pipewire.enable = true;
    flatpak.enable = true;
    # FIXME
    system-config-printer.enable = false;
    # tailscale.enable = true;
    pcscd = {
      enable = true;
    };

    avahi = {
      enable = true;
      nssmdns = true;
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
      (pkgs.writeTextFile {
        name = "platformio-udev";
        text =  builtins.readFile ./99-platformio-udev.rules;
        destination = "/etc/udev/rules.d/99-platformio-udev.rules";
      })
      # myUtsushi
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

    redis.servers."".enable = true;

    postgresql = {
      enable = true;
      package = pkgs.postgresql_13;
      settings."log_statement" = "all";
    };

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
      root.passwordFile = "/root/.passwd";
      abbradar = {
        passwordFile = "/root/.abbradar.passwd";
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "docker" "wireshark" "libvirtd" "cdrom" "vboxusers" ];
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
      pinentryFlavor = "gnome3";
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
      # enableSysbox = true;
    };
    libvirtd.enable = true;
    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };

  fileSystems."/media/nfs" = {
    fsType = "nfs";
    device = "abbradarserver.lan:/srv/files";
    options = [ "user" "noauto" ];
  };

}
