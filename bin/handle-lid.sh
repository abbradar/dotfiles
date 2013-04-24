#!/bin/bash

if [ $# != 2 ]; then
  echo "Close and open actions arguments are expected." >&2
  exit 1
fi

listen() {
  while read data; do
    if echo "$data" | grep -q "lid-is-closed: *yes"; then
      $1
    elif echo "$data" | grep -q "lid-is-closed: *no"; then
      $2
    fi
  done
}

upower --monitor-detail | listen "$1" "$2"