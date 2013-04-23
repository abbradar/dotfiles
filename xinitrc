#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

source ~/.profile
source ~/.xprofile

export DESKTOP_SESSION=kde
export XDG_CURRENT_DESKTOP=KDE

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrdb -merge ~/.Xresources &

exec systemd --user