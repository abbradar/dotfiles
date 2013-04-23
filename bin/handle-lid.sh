#!/bin/bash
LOCKPROC="slimlock"
LOCK="xautolock -locknow"

listen() {
  while read data; do
    if ! xlsclients | grep "$LOCKPROC"; then
      if echo "$data" | grep "lid-is-closed: *yes"; then
        $LOCK
      #elif echo "$data" | grep "lid-is-closed: *no"; then
      fi
    fi
  done
}

upower --monitor-detail | listen