{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
    };

    # Use chrooted builds.
    useChroot = true;

    #binaryCaches = [ "https://abbradar.net/hydra" ];
  };

  # Allow unfree packages.
  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableAdobeFlash = true;
      icedtea = true;
    };

    chromium = {
     enablePepperFlash = true;
     enablePepperPDF = true;
    };

    packageOverrides = self: with self; {
      pidgin-with-plugins = pidgin-with-plugins.override {
        plugins = [ pidginlatex pidginotr ];
      };
      haskellPackages = haskellPackages.override {
        extension = self: super: {
          xmonad-with-packages = super.xmonad-with-packages.override {
            packages = self: [ self.xmonadContrib self.xmonadExtras self.dbus self.h-booru ];
          };
        };
      };
    };
  };

  boot = {
    # Use the latest kernel version.
    kernelPackages = pkgs.linuxPackages_latest;

    cleanTmpDir = true;

    loader.efi.canTouchEfiVariables = true;
    loader.grub.version = 2;
  };

  # Time zone
  time.timeZone = "Europe/Moscow";

  # Security
  security = {
    sudo.configFile = ''
      Defaults rootpw,insults,timestamp_timeout=60
    '';
    rngd.enable = true;
  };

  # Select internationalization properties.
  i18n = {
    consoleKeyMap = "ruwin_cplk-UTF-8";
    defaultLocale = "en_US.UTF-8";
  };

  services = {
    xserver = {
      layout = "ru,us";
      xkbOptions = "eurosign:e,grp:caps_toggle,grp_led:scroll,terminate:ctrl_alt_bksp";
    };

    openssh = {
      passwordAuthentication = false;
    };

    journald.extraConfig = ''
      SystemMaxFileSize=5M
    '';
  };

  # Packages
  environment = {
    systemPackages = (with pkgs; [
      # Management
      smartmontools
      lm_sensors
      htop
      iotop
      nethogs
      psmisc
      lsof

      # Partitions
      wipe
      btrfsProgs
      hdparm

      # Files
      gptfdisk
      p7zip
      zip
      unzip
      unrar
      tree
      rsync
      file
      pv

      # Editors
      vim

      # Runtimes
      python3
      ruby_2_2
      jre
      mono

      # Encryption
      openssl
      gnupg

      # Develompent
      nix-repl
      nix-prefetch-scripts
      git
      subversion

      # Networking
      inetutils
      cifs_utils
      nfsUtils
      openvpn
      wget
      miniupnpc
      elinks

      # Utilities
      screen
      parallel
    ]);
  };

  # Enable OpenGL support.
  hardware = {
    opengl.driSupport32Bit = true;

    pulseaudio.configFile = ./default.pa;
  };

  # Zsh with proper path
  programs.zsh.enable = true;
}
