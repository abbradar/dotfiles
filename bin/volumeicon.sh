#!/bin/bash

watch_pulseaudio() {
  volumeicon &
  PID=$!
  while read data; do
    if echo "$data" | grep -q "^Event 'change' on server"; then
      echo "holy dicks"
      kill $PID
      volumeicon &
      PID=$!
    fi
  done
}

pactl subscribe | watch_pulseaudio
