#!/bin/bash

sudo sed -i "s/#ParallelDownloads = 5/ParallelDownloads = 5/" /etc/pacman.conf
sudo pacman -S --noconfirm alacritty git tmux firefox chromium zsh xclip \
zsh-autosuggestions net-tools noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-dejavu

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

git clone http://github.com/0xMoonrise/dotfiles

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
  sudo pacman -Syu

  curl "https://portswigger-cdn.net/burp/releases/download?product=community&version=2023.10.2.4&type=Linux" -o burp.sh
  echo 1ebf4100490799498073bac51bb71c2cb27ca48f642da9b495e65350d3f8b34d burp.sh | sha256sum -c 2>/dev/null
  if [[ $? -ne 0 ]]; then
    echo "[!] Warning burp.sh does not match with sha256sum."
    exit 1
  fi

fi
