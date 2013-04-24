#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

source ~/.profile
source ~/.xprofile
xrdb -merge ~/.Xresources

#export DESKTOP_SESSION=kde
#export XDG_CURRENT_DESKTOP=KDE
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/dbus/user_bus_socket"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

exec systemd --user
