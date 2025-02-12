# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{lib, ...}:
with lib.hm.gvariant; {
  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      show-warning = false;
    };

    "desktop/ibus/panel/emoji" = {
      hotkey = ["<Control>semicolon"];
    };

    "org/gnome/Connections" = {
      first-run = false;
    };

    "org/gnome/desktop/background" = {
      picture-options = "zoom";
      picture-uri = "file://${./wallpaper.png}";
      picture-uri-dark = "file://${wallpaper.png}";
    };

    "org/gnome/desktop/input-sources" = {
      mru-sources = [(mkTuple ["xkb" "us"]) (mkTuple ["xkb" "ru"])];
      per-window = false;
      xkb-options = ["grp:caps_toggle" "grp_led:scroll" "terminate:ctrl_alt_bksp" "compose:ralt"];
    };

    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      font-antialiasing = "rgba";
      font-hinting = "slight";
      gtk-theme = "Adwaita-dark";
      icon-theme = "Adwaita";
      show-battery-percentage = true;
    };

    "org/gnome/desktop/peripherals/mouse" = {
      accel-profile = "flat";
      speed = 0.55;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      disable-while-typing = false;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = false;
      dynamic-workspaces = true;
      edge-tiling = false;
      focus-change-on-pointer-rest = true;
      overlay-key = "Super_L";
      workspaces-only-on-primary = false;
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      disabled-extensions = [];
      enabled-extensions = ["caffeine@patapon.info" "paperwm@paperwm.github.com"];
      favorite-apps = ["firefox.desktop" "org.gnome.Nautilus.desktop" "org.gnome.Terminal.desktop"];
      remember-mount-password = false;
    };

    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };
  };
}
