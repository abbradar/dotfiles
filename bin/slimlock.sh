#!/bin/bash

systemctl --user -q is-active dunst-lock.service && DUNST_LOCKED=1
if ! [ $DUNST_LOCKED ]; then
  systemctl --user start dunst-lock.service
fi
slimlock
if ! [ $DUNST_LOCKED ]; then
  systemctl --user stop dunst-lock.service
fi
