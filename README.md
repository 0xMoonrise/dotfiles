# .config
This is my dot files

### Some tasks that I might need to configure
Change the network interface name:
```bash
sudo nano /etc/udev/rules.d/10-network.rules
```
add the content
```bash
SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="MAC", NAME="eth0"
SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="MAC", NAME="wlan0"
```
Create a backup from device:
```bash
sudo dd if=/dev/sdX bs=4M | pv -s $(sudo blockdev --getsize64 /dev/sdX) | gzip -9 > $(date --iso).img.gz
```
Restore a backup from `.img.gz`
```bash
zcat .img.gz | pv -s $(sudo blockdev --getsize64 /dev/sdX) |  dd of=/dev/sdX
```

Packages that might I use:
```
xclip
zsh-autosuggestions
net-tools
```

### Off-topic

```bash
cryptomount -a
insmod normal
normal
```
