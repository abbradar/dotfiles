#!/bin/bash

watch_pulseaudio() {
  kill_pulseaudio() {
    if [ "$vol_pid" != "" ]; then
      kill $vol_pid
    fi
  }
  trap kill_pulseaudio HUP INT QUIT TERM
  
  volumeicon &
  vol_pid=$!
  while read data; do
    if echo "$data" | grep -q "^Event 'change' on server"; then
      kill $vol_pid
      volumeicon &
      vol_pid=$!
    fi
  done
  kill $vol_pid
}

exec pactl subscribe | watch_pulseaudio