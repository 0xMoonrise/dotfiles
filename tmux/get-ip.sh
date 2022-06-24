#!/bin/bash
if [ -d /proc/sys/net/ipv4/conf/tun0 ]; then
    echo "$({ ip -4 -br a sh dev tun0 | awk {'print $3'} | cut -f1 -d/; } 2>/dev/null)"
else
    echo "$({ ip -4 -br a sh dev enp2s0 | awk {'print $3'} | cut -f1 -d/; } 2>/dev/null)"
fi
