#!/bin/bash

sudo sed -i "s/#ParallelDownloads = 5/ParallelDownloads = 5/" /etc/pacman.conf

sudo pacman -S --noconfirm - < base.txt

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

cp -R .config/*  ~/.config/
mv .bashrc .inputrc .nanorc .zshrc ~

./keybinds.pl keys-2023-10-04.bak

chsh -s $(which zsh)

if [[ $1 == "blackarch" ]]; then
  curl -O https://blackarch.org/strap.sh
  echo 26849980b35a42e6e192c6d9ed8c46f0d6d06047 strap.sh | sha1sum -c 2>/dev/null

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

  sudo pacman -S --noconfirm - < blacharch.txt
  
  sudo chown -R $USER:$USER /usr/share/netexec
  # TODO graphic drivers to enable crack station with john and hashcat
  # TODO download wifi drivers https://gist.github.com/watzon/dabdc4cef0f0b3be9bc34a5a5c2686f9
fi
