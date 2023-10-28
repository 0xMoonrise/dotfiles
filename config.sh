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

git clone http://github.com/0xMoonrise/dotfiles
mv alacritty ~/.config/
mv tmux ~/.config/
mv .bashrc .inputrc .nanorc .zshrc ~

./keybinds.pl keys-2023-10-04.bak

chsh -s $(which zsh)
