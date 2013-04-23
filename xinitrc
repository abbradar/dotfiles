#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

#KDEMODULES=(kscreen bluedevil solidautoeject device_automounter kmixd wacomtablet)
KDEMODULES=(kscreen device_automounter wacomtablet bluedevil)

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

kde_load() {
  #klipper &
  #synaptiks &
  bluedevil-monolithic &
  #/usr/lib/kde4/libexec/polkit-kde-authentication-agent-1 &

  kdeinit4
  for i in ${KDEMODULES[@]}; do
    qdbus org.kde.kded /kded org.kde.kded.loadModule $i
  done
}

setxkbmap -layout us,ru -option grp:caps_toggle -option terminate:ctrl_alt_bksp

kde_load &
xrdb -merge ~/.Xresources &
~/bin/handle_lid.sh &
gnome-keyring &
nitrogen --restore &
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 16 --transparent true --tint 0x191970 --height 14 \
    --monitor primary &
xautolock -time 10 -locker "notify-send DUNST_COMMAND_PAUSE; slimlock; notify-send DUNST_COMMAND_RESUME" -detectsleep -resetsaver &
xxkb &
volumeicon &
xmenud -t &
#twmnd &
dunst &
dex -a -e LXDE &

systemd-inhibit --what=sleep:handle-power-key:handle-suspend-key:handle-hibernate-key:handle-lid-switch xmonad
