#!/usr/bin/env bash

set -eo pipefail

usage() {
  echo "$0 {connect|disconnect}"
  exit 1
}

# Mullvad's firewall drops everything that is not its own tunnel traffic, except for packets
# whose conntrack mark is `split_tunnel::MARK`. Anything that needs to live outside the Mullvad
# tunnel punches its own hole with that mark; this script only does so for Tailscale.
SPLIT_TUNNEL_MARK=0x00000f41

if [ "$1" = "connect" ]; then
  mullvad connect -w
  trap "mullvad disconnect -w" ERR
  # NOTE: the relative ordering of the IP rules is set by the patched Mullvad daemon, which
  # installs its rules at priority 5280 instead of 0. See 0001-Set-base-rule-priority.patch.
  nft -f - <<EOF
table inet mullvad-tailscale {
  chain fix-tailscale {
    # Runs before Mullvad's mangle chain (priority -150) and its output filter chain
    # (priority 0); at priority 0 the ordering against the latter would be registration order.
    type route hook output priority -151; policy accept;
    # Allow all traffic inside Tailscale to bypass the firewall.
    oif tailscale0 ct mark set $SPLIT_TUNNEL_MARK;
    # Allow all Tailscale Internet traffic to bypass the firewall.
    meta mark & 0xff0000 == 0x80000 ct mark set $SPLIT_TUNNEL_MARK;
  }
}
EOF
elif [ "$1" = "disconnect" ]; then
  mullvad disconnect -w
  nft delete table inet mullvad-tailscale || true
else
  usage
fi
