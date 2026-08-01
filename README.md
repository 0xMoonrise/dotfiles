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
mkinitcpio-firmware
```

Disable GDM Login Screen Suspend on AC Power
```bash
sudo mkdir -p /etc/dconf/db/gdm.d
sudo tee /etc/dconf/db/gdm.d/00-no-sleep-ac <<'EOF'
[org/gnome/settings-daemon/plugins/power]
sleep-inactive-ac-type='nothing'
sleep-inactive-ac-timeout=0
EOF

sudo dconf update
sudo systemctl restart gdm
```

Disable Sleep on AC Power Logged-In Session
```bash
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type 'nothing'
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 0
```

**Note:** For lid-close behavior, this needs to be paired separately
with `HandleLidSwitchExternalPower=ignore` in `/etc/systemd/logind.conf`.

### Off-topic

```bash
cryptomount -a
insmod normal
normal
```
