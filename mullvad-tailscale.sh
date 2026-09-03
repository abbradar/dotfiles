#!/usr/bin/env bash

set -eo pipefail

usage() {
  echo "$0 {connect|disconnect}"
  exit 1
}

# Mullvad's firewall drops everything that is not its own tunnel traffic, except for packets
# whose conntrack mark is `split_tunnel::MARK`. We reuse that mark to punch holes for the
# tunnels layered around Mullvad.
SPLIT_TUNNEL_MARK=0x00000f41
# FwMark of the first-layer wg-quick tunnel that carries Mullvad itself, see
# ~/proxy-shanghai.conf.
PROXY_FWMARK=0xca6c

if [ "$1" = "connect" ]; then
  mullvad connect -w
  trap "mullvad disconnect -w" ERR
  # NOTE: the IP rule ordering (Tailscale first, then Mullvad, then proxy-shanghai) is set up
  # by the patched Mullvad daemon, which installs its rules at priority 5280 instead of 0.
  # See 0001-Set-base-rule-priority.patch.

  # Mullvad publishes `~.` as its DNS routing domain. Drop proxy-shanghai's so that
  # systemd-resolved does not also query 172.16.47.1, which Mullvad's firewall blocks.
  resolvectl domain proxy-shanghai '' || true

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
    # Allow the first-layer WireGuard tunnel, which Mullvad itself runs on top of.
    meta mark $PROXY_FWMARK ct mark set $SPLIT_TUNNEL_MARK;
  }
}
EOF
elif [ "$1" = "disconnect" ]; then
  mullvad disconnect -w
  nft delete table inet mullvad-tailscale || true
  resolvectl domain proxy-shanghai '~.' || true
else
  usage
fi
