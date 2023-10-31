#!/usr/bin/env python3
import socket
import fcntl
import struct
import glob
import os

def get_ip_address(ifname):
    ifname_bytes = ifname.encode()
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        return socket.inet_ntoa(fcntl.ioctl(
            s.fileno(),
            0x8915,
            struct.pack('256s', ifname_bytes[:15]))[20:24])
    except OSError:
        return None

def find_available_interface():
    potential_interfaces = ['tun*', 'eth*', 'enp*', 'wlan*', 'wlp*']
    for interface_pattern in potential_interfaces:
        interfaces = glob.glob(f'/sys/class/net/{interface_pattern}')
        for interface in interfaces:
            interface_name = os.path.basename(interface)
            ip = get_ip_address(interface_name)
            if ip:
                return ip

    return None, None

ip = find_available_interface()
if ip:
    print(ip)
else:
    print("Error")
