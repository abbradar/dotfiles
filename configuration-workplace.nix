{ config, pkgs, ... }:

{
  imports =
    [ /home/abbradar/dotfiles/configuration-common.nix
    ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    source-code-pro
    cm_unicode
  ];

  nix.nixPath = [ "nixpkgs=/home/abbradar/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];

  nixpkgs.config = {
    android_sdk.accept_license = true;
    # Build packages with pulseaudio support
    pulseaudio = true;
  };

  boot = {
    loader.timeout = 0;
    supportedFilesystems = [ "nfs" "ntfs" "exfat" ];
    plymouth.enable = true;
  };

  networking = {
    firewall.enable = false;
    wireguard.enable = true;
  };

  environment.systemPackages = with pkgs; [
    # Utils
    powertop
    s-tui
    minicom
    mercurial
    google-cloud-sdk
    pavucontrol
    androidenv.androidPkgs_9_0.platform-tools
    platformio
    silver-searcher

    # Runtimes
    steam-run-native
    steam
    lgogdownloader
    wineWowPackages.staging
    lutris
    openjdk11
    leiningen
    icedtea_web

    # VM
    virtmanager

    # Multimedia
    firefox
    chromium
    (deadbeef-with-plugins.override { 
      plugins = [ deadbeef-mpris2-plugin ]; 
    })
    thunderbird
    mpv
    youtube-dl
    syncplay
    gimp
    inkscape
    zathura

    # Messengers
    #dino
    gajim
    riot-desktop
    tdesktop
    signal-desktop
    mumble_git

    # Development
    vscode
    emacs
    #irony-server
    cabal-install
    stack
    cabal2nix
    nox

    # Network
    deluge
    miniupnpc
    wget
    openvpn
    remmina
    shadowsocks-libev
    tor

    # Documents
    libreoffice
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
  ];

  i18n = {
    consolePackages = [ pkgs.terminus_font ];
    inputMethod.enabled = "ibus";
  };

  sound.enable = true;

  hardware = {
    pulseaudio.enable = true;
    sane.enable = true;
  };

  services = {
    teamviewer.enable = true;

    avahi.enable = true;
    printing = {
      enable = true;
      drivers = with pkgs; [ epson-escpr gutenprint ];
    };

    udev.packages = with pkgs; [
      android-udev-rules
      libmtp
      (pkgs.writeTextFile {
        name = "platformio-udev";
        text =  builtins.readFile ./99-platformio-udev.rules;
        destination = "/etc/udev/rules.d/99-platformio-udev.rules";
      })
    ];

    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      #displayManager.gdm.wayland = false;
      desktopManager.gnome3 = {
        enable = true;
        sessionPath = with pkgs.gnomeExtensions; [
          #caffeine
          appindicator
          topicons-plus
       ];
      };
    };

    postgresql = {
      enable = true;
      extraConfig = ''
        log_statement = all
      '';
    };

    samba = {
      enable = true;
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
        extraGroups = [ "wheel" "docker" "wireshark" "libvirtd" "cdrom" ];
      };
    };
  };

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

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

  security.wrappers."mount.nfs" = {
    source = "${pkgs.nfs-utils}/bin/mount.nfs";
  };

  virtualisation = {
    docker = {
      enable = true;
      storageDriver = "btrfs";
    };
    libvirtd.enable = true;
    virtualbox.host = {
      enable = true;
    };
  };

  fileSystems."/media/nfs" = {
    fsType = "nfs";
    device = "abbradarserver.lan:/srv/files";
    options = [ "user" "noauto" ];
  };

}
