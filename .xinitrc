#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

DEBUG=0

[ -f /etc/xprofile ] && source /etc/profile
[ -f ~/.profile ] && source ~/.profile
[ -f ~/.xprofile ] && source ~/.xprofile

export XDG_RUNTIME_DIR="/run/user/$(id -u)/$$"
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/dbus/user_bus_socket"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    # To work around pulseaudio script in Arch
    [ -x "$f" ] && [ "$(basename "$f")" != 'pulseaudio' ] && . "$f"
  done
  unset f
fi

cleanup() {
  [ "$DEBUG" = "0" ] && kill $PID
  rm -rf "$XDG_RUNTIME_DIR"
}
trap cleanup HUP INT QUIT TERM

if [ "$DEBUG" = "0" ]; then
  /usr/lib/systemd/systemd --user &

  PID=$!
  ( waitx; kill $PID ) &
  wait $PID
  PID=
else
  urxvt
fi

cleanup
