#!/bin/bash

COMPONENTS="gpg,pkcs11,secrets,ssh"
VARIABLES=(SSH_AUTH_SOCK GPG_AGENT_INFO GNOME_KEYRING_CONTROL GNOME_KEYRING_PID)

if [ $# != 1 ]; then
  echo "usage: $0 [start|cleanup]"
  exit 1
fi

if [ "$1" == "start" ]; then
  eval $(/usr/bin/gnome-keyring-daemon --start -c $COMPONENTS)
  [ "$?" = "0" ] || exit 1

  for i in "${VARIABLES[@]}"; do
    systemctl --user set-environment $i=$(eval "echo \$$i")
  done
elif [ "$1" == "cleanup" ]; then
  systemctl --user unset-environment "${VARIABLES[@]}"
else
  echo "usage: $0 [start|cleanup]"
  exit 1
fi
