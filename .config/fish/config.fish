# Main fish settings
set fish_greeting ""

function fish_prompt
  set dir (basename $PWD)
  if test $PWD = $HOME
    set dir "~"
  end
    echo ["$USER"@$hostname $dir]
    echo "\$ "
end

set -e GOROOT
set -gx PATH $PATH /usr/local/go/bin
set -gx GOPATH $HOME/go

set -x PKG_CONFIG_PATH /usr/local/lib/pkgconfig $PKG_CONFIG_PATH
# quality of life aliases

# useful variables
set -gx TERM xterm-256color

# functions and utility code
function set-inet
  tmux set-option -g update-environment "INET $argv[1]"
end

function download_pdf
  wget $argv -P /opt/lectures/
end

function upload_archive
  curl -v -X POST http://localhost:8080/upload -F "file=@$argv"
end

if status is-interactive
and not set -q TMUX
    if tmux has-session -t base 2>/dev/null
        tmux attach
    else
        tmux new -s base
    end
end

function envsource
  set -f envfile "$argv"
  if not test -f "$envfile"
    echo "Unable to load $envfile"
    return 1
  end
  while read line
    if not string match -qr '^#|^$' "$line"
      set item (string split -m 1 '=' $line)
      set -gx $item[1] $item[2]
      echo "Exported key $item[1]"
    end
  end < "$envfile"
end

function open_crypto
  set disk "$argv[1]"
  sudo cryptsetup luksOpen $disk vault
  sudo mount /dev/mapper/vault /mnt/usb/  
  sudo chown -R 1000:1000 /mnt/usb/
end

function close_crypto
  set disk "$argv[1]"
  sudo umount /mnt/usb/
  sudo cryptsetup luksClose /dev/mapper/vault
  sudo eject $disk
end

# Created by `pipx` on 2025-12-26 20:11:50
set PATH $PATH /home/moonrise/.local/bin

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
