source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
setopt interactivecomments
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

plugins=(
  zsh-autosuggestions
)
bindkey '^\033[1;5C' autosuggest-accept
bindkey "^\033[1;5C" forward-word
bindkey "^\033[1;5D" backward-word
PROMPT='[%n@%m %1~]%# '
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias st_htb='sudo openvpn ~/.vpn/starting_point_0xMoonrise.ovpn'
alias htb='sudo openvpn ~/.vpn/lab_0xMoonrise.ovpn'
alias thm='sudo openvpn ~/.vpn/m4ll0c.ovpn'
alias open='xdg-open'
alias nnano='tmux new-window nano'
alias hnano='tmux split-window -h nano'
alias vnano='tmux split-window -v nano'
alias gnome-config='nano ~/.config/gtk-3.0/gtk.css'
alias tmux-config='nano ~/.config/tmux/tmux.conf'
alias bash-config='nano ~/.bashrc && source ~/.bashrc'

#Environment variables for usability
#$export PATH="/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/opt/metasploit/tools/exploit/"
#$export PATH="$PATH:/home/moon/.local/share/gem/ruby/3.0.0/bin"
export WEB_CONENT="/usr/share/seclists/Discovery/Web-Content/"
export DNS="/usr/share/seclists/Discovery/DNS/"
export LEAK_DB="/usr/share/seclists/Passwords/Leaked-Databases"
export VISUAL=nano
export EDITOR=nano
export PIP_BREAK_SYSTEM_PACKAGES=1
export LS_COLORS="$LS_COLORS:*.tar=1;31:*.tgz=1;31:*.arj=1;31:*.taz=1;31:*.lzh=1;31:*.lzma=1;31:*.tlz=1;31:*.txz=1;31:*.zip=1;31:*.z=1;31:*.Z=1;31:*.dz=1;31:*.gz=1;31:*.lz=1;31:*.xz=1;31:*.bz2=1;31:*.bz=1;31:*.tbz=1;31:*.tbz2=1;31:*.tz=1;31:*.deb=1;31:*.rpm=1;31:*.jar=1;31:*.war=1;31:*.ear=1;31:*.sar=1;31:*.rar=1;31:*.ace=1;31:*.zoo=1;31:*.cpio=1;31:*.7z=1;31:*.rz=1;31"

set_target()
{
    tmux set-environment target $1
}

get_target()
{
    tmux show-environment target | awk -F '=' '{print $2}'
}

httpserver()
{
    [[ -z $1 ]] && port=8000 || port=$1
    local INET=''

    if [[ -f /sys/class/net/tun0/operstate ]]; then
        INET='tun0'
    elif [[ $(< /sys/class/net/eth0/operstate) == "up" ]]; then
        INET='eth0'
    else
        INET='wlan0'
    fi

    local IP=$(ip -4 addr show $INET | grep -oP "(?<=inet\s)\d+(\.\d+){3}")
    echo "http://$IP:$port/"
    python -m http.server $port 1>/dev/null
}

