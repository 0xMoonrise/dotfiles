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
alias server='python -m http.server'
alias open='xdg-open'

PS1='[\u@\h \W]\$ '

#Environment variables for usability
export PATH="/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/opt/metasploit/tools/exploit/"
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
    echo "http://$(ip -4 addr show eth0 | sed -n 's/.*inet \(.*\)\/24 brd.*/\1/p'):$port/"
    python -m http.server $port 1>/dev/null
}
