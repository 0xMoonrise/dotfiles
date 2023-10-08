#!/bin/bash

inets=$(ip addr | awk '/^[0-9]+: (.*):/ {print $2}')
if [[ $inets =~ tun[0-9]+   ||
      $inets =~ wlan[0-9]+  ||
      $inets =~ eth[0-9]+   ||
      $inets =~ enp.*[0-9]+ ]]; then
    echo $(ip -4 addr show "${BASH_REMATCH[0]}" | grep -oP "(?<=inet\s)\d+(\.\d+){3}")
else
    echo "[inet not found]"
fi
