#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

[ -f /etc/xprofile ] && source /etc/profile
[ -f ~/.profile ] && source ~/.profile
[ -f ~/.xprofile ] && source ~/.xprofile
xrdb -merge ~/.Xresources

export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/dbus/user_bus_socket"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    # To work around pulseaudio script in Arch
    [ -x "$f" ] && [ "$(basename "$f")" != 'pulseaudio' ] && . "$f"
  done
  unset f
fi

/usr/lib/systemd/systemd --user &
PID=$!

( waitx; kill $PID ) &
wait $PID
