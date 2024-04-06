#!/bin/bash

sudo sed -i "s/#ParallelDownloads = 5/ParallelDownloads = 5/" /etc/pacman.conf

sudo pacman -S --noconfirm alacritty git tmux firefox chromium zsh xclip \
zsh-autosuggestions net-tools noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-dejavu \
python-pip micro

python -m pip install setuptools

echo '<alias>
  <family>sans-serif</family>
  <prefer>
    <family>Noto Sans</family>
  </prefer>
</alias>' > /etc/fonts/local.conf

fc-cache -fv

gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
gsettings set org.gnome.desktop.background picture-options 'none'
gsettings set org.gnome.desktop.background primary-color '#222d33'

gsettings set org.gnome.shell.keybindings switch-to-application-1 \[\]
gsettings set org.gnome.shell.keybindings switch-to-application-2 \[\]
gsettings set org.gnome.shell.keybindings switch-to-application-3 \[\]
gsettings set org.gnome.shell.keybindings switch-to-application-4 \[\]

mv alacritty ~/.config/
mv tmux ~/.config/
mv .bashrc .inputrc .nanorc .zshrc ~

./keybinds.pl keys-2023-10-04.bak

chsh -s $(which zsh)

if [[ $1 == "blackarch" ]]; then
  curl -O https://blackarch.org/strap.sh
  echo 5ea40d49ecd14c2e024deecf90605426db97ea0c strap.sh | sha1sum -c 2>/dev/null

  if [[ $? -ne 0 ]]; then
    echo "[!] Warning strap.sh does not match with sha1sum."
    exit 1
  fi

  echo "[+] Success..."
  chmod +x strap.sh
  sudo ./strap.sh
  sudo pacman -Syu --noconfirm

  curl "https://portswigger-cdn.net/burp/releases/download?product=community&type=Linux" -O burp.sh

  echo "[+] Success..."

  sudo pacman -S --noconfirm wfuzz ffuf metasploit exploitdb ghidra radare2 evil-winrm crackmapexec \
  jwt-tool nmap zmap dirbuster dirsearch hashcat john hashid httpx amass tcpdump wireshark-qt \
  wireshark-cli aircrack-ng seclists openvpn keepass nuclei assetfinder dirb arjun gospider \
  waybackurls gau hakrawler nikto netstumbler netcat sqlmap ettercap rainbowcrack arp-scan \
  
  sudo chown -R $USER:$USER /usr/share/crackmapexec
  # TODO graphic drivers to enable crack station with john and hashcat
  # TODO download wifi drivers https://gist.github.com/watzon/dabdc4cef0f0b3be9bc34a5a5c2686f9
fi
