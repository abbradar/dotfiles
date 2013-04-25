#!/bin/bash

if [ $# -lt 3 ]; then
    echo "Wrong number of args: $#"
    exit -1
fi

#the output name (eg: HDMI1, LVDS1, etc)
output_name="$1"
#either Connected or Disconnected
connection_state="$2"
#resolution as a string (eg: 1366x768), or None if output isn't currently enabled
mode_name="$3"

if [ ${connection_state} = "Connected" -a ${mode_name} != "None" ] \
    || [ ${connection_state} = "Disconnected" -a ${mode_name} == "None" ]; then
  systemctl --user restart display-set.target
fi