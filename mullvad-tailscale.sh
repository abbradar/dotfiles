#!/usr/bin/env bash

set -euo pipefail

# Mullvad's firewall drops everything outside its tunnel except packets whose conntrack mark is
# `split_tunnel::MARK`; this script sets it on Tailscale traffic. Routing needs no changes here:
# the patched daemon installs its IP rules at priority 5280 (see 0001-Set-base-rule-priority.patch).
SPLIT_TUNNEL_MARK=0x00000f41
# TAILSCALE_INTERFACE is set by the Nix wrapper from `services.tailscale.interfaceName`.

case "${1:-}" in
  connect)
    mullvad connect -w
    trap "mullvad disconnect -w" ERR
    nft -f - <<EOF
destroy table inet mullvad-tailscale
table inet mullvad-tailscale {
  chain fix-tailscale {
    # Must run after Tailscale's iptables mangle/OUTPUT, whose "CONNMARK --save-mark --nfmask
    # 0xff0000 --ctmask 0xff0000" on new connections would turn our ct mark into 0x80f41, and
    # before Mullvad's filter chains (priority 0), which match ct mark == 0xf41 exactly.
    type filter hook output priority mangle + 1; policy accept;
    # Allow all traffic inside Tailscale to bypass the firewall.
    oif "$TAILSCALE_INTERFACE" ct mark set $SPLIT_TUNNEL_MARK;
    # Allow all Tailscale Internet traffic to bypass the firewall.
    meta mark & 0xff0000 == 0x80000 ct mark set $SPLIT_TUNNEL_MARK;
  }
}
EOF
    ;;
  disconnect)
    mullvad disconnect -w
    nft destroy table inet mullvad-tailscale
    ;;
  *)
    echo "$0 {connect|disconnect}"
    exit 1
    ;;
esac
