#!/bin/bash

DUNST_LOCKED=$(systemctl --user is-active dunst-lock.service)
if ! $DUNST_LOCKED; then
  systemctl start dunst-lock.service
fi
slimlock
if ! $DUNST_LOCKED; then
  systemctl stop dunst-lock.service
fi
