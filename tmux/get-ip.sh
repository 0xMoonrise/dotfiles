#!/bin/bash

INET=''
name=('tun0' 'eth0' 'wlan0')
if [[ -f /sys/class/net/${name[1]}/operstate ]]; then
    INET=${name[1]}
elif [[ $(< /sys/class/net/${name[2]}/operstate) == "up" ]]; then
    INET=${name[2]}
else
    INET=${name[3]}
fi

echo $(ip -4 addr show $INET | grep -oP "(?<=inet\s)\d+(\.\d+){3}")

