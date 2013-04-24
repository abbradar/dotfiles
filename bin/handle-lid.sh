#!/bin/bash

listen() {
  while read data; do
    if echo "$data" | grep "lid-is-closed: *yes"; then
      systemctl --user start lid-close.target
    elif echo "$data" | grep "lid-is-closed: *no"; then
      systemctl --user stop lid-close.target
    fi
  done
}

upower --monitor-detail | listen