#!/bin/bash

watch_setxkbmap() {
  kill_setxkbmap() {
    if (( $LASTTIME + 2 >= $(date +%s) )); then
      kill $LASTID
      wait $LASTID
    fi
  }
  trap kill_setxkbmap HUP INT QUIT TERM
  
  setxkbmap $OPTIONS
  while read data; do
    if echo "$data" | grep -q " add "; then
      if (( $LASTTIME + 2 >= $(date +%s) )); then
        kill $LASTID
      fi
      ( sleep 0.1; setxkbmap $OPTIONS ) &
      LASTID="$!"
      LASTTIME="$(date +%s)"
    fi
  done
  kill_setxkbmap
}

export OPTIONS=$@
exec unbuffer udevadm monitor --udev --subsystem-match=input | watch_setxkbmap
