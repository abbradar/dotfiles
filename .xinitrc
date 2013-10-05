#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

DEBUG=0

echo "starting systemd, self pid $$, debug $DEBUG" >> /tmp/test.log

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
  echo "cleaning up started" >> /tmp/test.log
  if [[ "$DEBUG" = "0" && "$PID" != "" ]]; then
    echo "killing systemd" >> /tmp/test.log
    kill $PID
    wait $PID
    echo "cleaned up" >> /tmp/test.log
  fi
  rm -rf "$XDG_RUNTIME_DIR"

  echo "cleaning up finished, exiting" >> /tmp/test.log
  exit 0
}
trap cleanup HUP INT QUIT TERM

if [[ "$DEBUG" = "0" ]]; then
  /usr/lib/systemd/systemd --user &
  PID=$!

  echo "waiting for systemd, pid $PID" >> /tmp/test.log
  wait $PID
  PID=
else
  urxvt
fi

cleanup
