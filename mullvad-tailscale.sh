#!/usr/bin/env bash

set -eo pipefail

usage() {
  echo "$0 {connect|disconnect}"
  exit 1
}

if [ "$1" = "connect" ]; then
  mullvad connect -w
  trap "mullvad disconnect -w" ERR
  # Move the catch-all IP rule down so Tailscale is routed first.
  ip rule del not from all fwmark 0x6d6f6c65 lookup 1836018789 || true
  ip rule add not from all fwmark 0x6d6f6c65 lookup 1836018789 prio 5300
  nft -f - <<EOF
table inet mullvad-tailscale {
  chain fix-tailscale {
    type route hook output priority 0; policy accept;
    # Allow all traffic inside Tailscale to bypass the firewall.
    oif tailscale0 ct mark set 0x00000f41;
    # Allow all Tailscale Internet traffic to bypass the firewall.
    meta mark & 0xff0000 == 0x80000 ct mark set 0x00000f41;
  }
}
EOF
elif [ "$1" = "disconnect" ]; then
  mullvad disconnect -w
  nft delete table inet mullvad-tailscale || true
else
  usage
fi
