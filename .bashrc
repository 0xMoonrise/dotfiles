export TERM=xterm-256color
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

path_add() {
    local dir="$1"

    [[ -d "$dir" ]] || return

    case ":$PATH:" in
        *":$dir:"*) return ;;
    esac

    PATH="$PATH:$dir"
}

path_add /usr/local/go/bin
path_add "$HOME/go/bin"

export PATH

PS1='[\u@\h \W]\n\$ '

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

alias emacs='emacsclient --tty'
alias emacsv='emacsclient --create-frame'
alias ls='ls --color=auto'

open_crypto() {
    local disk="$1"
    local mapper="vault"
    local mountpoint="/mnt/usb"

    if [[ -z "$disk" ]]; then
        echo "Usage: open_crypto <device>"
        return 1
    fi

    if [[ -e "/dev/mapper/$mapper" ]]; then
        echo "LUKS device '$mapper' is already open"
    else
        sudo cryptsetup luksOpen "$disk" "$mapper" || return 1
    fi

    if mountpoint -q "$mountpoint"; then
        echo "Vault is already mounted at $mountpoint"
        return 0
    fi

    sudo mount "/dev/mapper/$mapper" "$mountpoint" || {
        echo "Error: failed to mount $mountpoint"
        return 1
    }

    echo "Vault mounted at $mountpoint"
}

close_crypto() {
    local disk="$1"
    local mapper="vault"
    local mountpoint="/mnt/usb"

    if mountpoint -q "$mountpoint"; then
        sudo umount "$mountpoint" || return 1
    fi

    if [[ -e "/dev/mapper/$mapper" ]]; then
        sudo cryptsetup luksClose "$mapper" || return 1
    fi

    if [[ -n "$disk" ]]; then
        sudo udisksctl power-off -b "$disk" || return 1
    fi

    echo "Vault closed and device powered off"
}

ps-mem() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: ps-mem <PID>"
        return 1
    fi

    local pid="$1"

    if ! ps -p "$pid" > /dev/null 2>&1; then
        echo "Process $pid does not exist"
        return 1
    fi

    ps -p "$pid" \
        -o pid,comm,rss,size,vsize,pmem,time \
        --no-headers |
    while read -r pid comm rss size vsize pmem time; do
        printf "PID: %-6s\n" "$pid"
        printf "Command: %-10s\n" "$comm"
        printf "RSS: %-8s\n" "$(numfmt --to=iec "$(("$rss" * 1024))" 2>/dev/null)"
        printf "SIZE: %-8s\n" "$(numfmt --to=iec "$(("$size" * 1024))" 2>/dev/null)"
        printf "VIRT: %-8s\n" "$(numfmt --to=iec "$(("$vsize" * 1024))" 2>/dev/null)"
        printf "%%MEM: %-4s%%\n" "$pmem"
        printf "TIME: %s\n" "$time"
    done
}


if [[ $- == *i* ]] &&
   [[ -z "$TMUX" ]] &&
   {
       [[ "$HOSTNAME" == "server-01" ]] ||
       [[ -n "$SSH_CONNECTION" ]]
   }
then
    if tmux has-session -t base 2>/dev/null; then
        tmux attach-session -t base
    else
        tmux new-session -s base
    fi
fi

. "/home/moonrise/.deno/env"
source /home/moonrise/.local/share/bash-completion/completions/deno.bash
export PATH="$HOME/.deno/bin:$PATH"
