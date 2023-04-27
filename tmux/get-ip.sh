#!/bin/bash

INET=''

if [[ -f /sys/class/net/tun0/operstate ]]; then
    INET='tun0'
elif [[ $(< /sys/class/net/eth0/operstate) == "up" ]]; then
    INET='eth0'
else
    INET='wlan0'
fi

echo $(ip -4 addr show $INET | grep -oP "(?<=inet\s)\d+(\.\d+){3}")

