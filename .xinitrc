#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

DEBUG=0

USERDIR="/run/user/$(id -u)"
RUNDIR="$USERDIR/$$"
LOGFILE="$USERDIR/xinitrc.$$.log"

mkdir -p "$RUNDIR" || exit 1
echo "starting systemd, self pid $$, debug $DEBUG" >> "$LOGFILE"

[ -f /etc/xprofile ] && source /etc/profile
[ -f ~/.profile ] && source ~/.profile
[ -f ~/.xprofile ] && source ~/.xprofile

export XDG_RUNTIME_DIR="$RUNDIR"
export DBUS_SESSION_BUS_ADDRESS="unix:path=$RUNDIR/dbus/user_bus_socket"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    # To work around pulseaudio script in Arch
    [ -x "$f" ] && [ "$(basename "$f")" != 'pulseaudio' ] && . "$f"
  done
  unset f
fi

cleanup() {
  echo "cleaning up started" >> "$LOGFILE"
  if [[ "$DEBUG" = "0" && "$PID" != "" ]]; then
    echo "killing systemd" >> "$LOGFILE"
    kill $PID
    wait $PID
  fi
  rm -rf "$XDG_RUNTIME_DIR" || echo "failed to clean up folders" >> "$LOGFILE"

  echo "cleaning up finished, exiting" >> "$LOGFILE"
  exit 0
}
trap cleanup HUP INT QUIT TERM

if [[ "$DEBUG" = "0" ]]; then
  /usr/lib/systemd/systemd --user &
  PID=$!

  echo "waiting for systemd, pid $PID" >> "$LOGFILE"
  wait $PID
  PID=
else
  urxvt
fi

cleanup
