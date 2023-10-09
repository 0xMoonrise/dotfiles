#/bin/bash

inets=$(ip addr | awk '/^[0-9]+: (.*):/ {print $2}')

if [[ $inets =~ tun[0-9]+  && $(< /sys/class/net/${BASH_REMATCH[0]}/operstate) == "up" ]]; then
        inet="${BASH_REMATCH[0]}"
elif [[ $inets =~ eth[0-9]+|enp.*[0-9]+  && $(< /sys/class/net/${BASH_REMATCH[0]}/operstate) == "up" ]]; then
        inet="${BASH_REMATCH[0]}"
elif [[ $inets =~ wlan[0-9]+  && $(< /sys/class/net/${BASH_REMATCH[0]}/operstate) == "up" ]]; then
        inet="${BASH_REMATCH[0]}"
else
        echo [no inet or ip]
        exit 1
fi

echo $(ip -4 addr show $inet | grep -oP "(?<=inet\s)\d+(\.\d+){3}")
