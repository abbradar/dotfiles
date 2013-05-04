#!/bin/bash
activeWinId=$(xprop -root | sed -e '/^_NET_ACTIVE_WINDOW(WINDOW)/!d; s/.*\(0x[0-9abcde]\+\).*/\1/g')
import -window "$activeWinId" "$1"
