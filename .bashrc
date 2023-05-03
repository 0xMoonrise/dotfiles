#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#Useful aliases
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
PS1='[\u@\h \W]\$ '

#Environment variables for usability
export PATH="/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/opt/metasploit/tools/exploit/"
export PATH="$PATH:/home/moon/.local/share/gem/ruby/3.0.0/bin"
export WEB_CONENT="/usr/share/seclists/Discovery/Web-Content/"
export DNS="/usr/share/seclists/Discovery/DNS/"
export LEAK_DB="/usr/share/seclists/Passwords/Leaked-Databases"
export VISUAL=nano
export EDITOR=nano

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
