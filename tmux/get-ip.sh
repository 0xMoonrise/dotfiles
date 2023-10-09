#/bin/bash

ip -o -4 addr show | awk '/tun[0-9]+/ {print $4; exit}
			  /eth[0-9]+|enp.*[0-9]+/ {print $4; exit}
			  /wlan[0-9]+/ {print $4; exit}'
