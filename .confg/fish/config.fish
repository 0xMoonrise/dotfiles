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

# quality of life aliases

# useful variables
set -gx TERM xterm-256color
set -gx INET eth0
set -gx PATH $PATH /usr/local/go/bin
set -gx GOPATH $HOME/go
set -xg GOOS linux
set -xg GOARCH arm64

# functions and utility code
function set-inet
    tmux set-environment INET "$argv[1]"
end

function download_pdf
  wget $argv -P /opt/lectures/
end

function upload_archive
  curl -v -X POST http://localhost:8080/upload -F "file=@$argv"
end

if status is-interactive
and not set -q TMUX
  tmux attach -t base || tmux new -s base
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

# Generated for envman. Do not edit.
test -s ~/.config/envman/load.fish; and source ~/.config/envman/load.fish
