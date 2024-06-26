# Zsh Autocompletion
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Zsh Options
setopt interactivecomments
setopt interactivecomments
setopt histignorealldups
setopt sharehistory
autoload -Uz compinit
autoload -U history-search-end
compinit
zstyle ':completion:*' menu select
zstyle ":completion:*:commands" rehash 1

# Command History Settings
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# Zsh Plugins
plugins=(
  zsh-autosuggestions
)

# Keyboard Shortcuts
bindkey '^[[Z' autosuggest-accept
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^\033[1;5C" forward-word
bindkey "^\033[1;5D" backward-word

# Prompt Configuration
PROMPT='[%n@%m %1~]%# '

# Check if it's an interactive session
[[ $- != *i* ]] && return

# Useful Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias open='xdg-open'

# VPN Connection Aliases
alias st_htb='sudo openvpn ~/.vpn/starting_point_0xMoonrise.ovpn'
alias htb='sudo openvpn ~/.vpn/lab_0xMoonrise.ovpn'
alias thm='sudo openvpn ~/.vpn/m4ll0c.ovpn'

# Tmux Split Window Aliases
alias nnano='tmux new-window nano'
alias hnano='tmux split-window -h nano'
alias vnano='tmux split-window -v nano'

# Edit Configuration Files Aliases
alias gnome-config='nano ~/.config/gtk-3.0/gtk.css'
alias tmux-config='nano ~/.config/tmux/tmux.conf'
alias bash-config='nano ~/.bashrc && source ~/.bashrc'
alias zsh-config='nano ~/.zshrc && source ~/.zshrc'

# Environment Variable to set delimiter
export WORDCHARS='*?_[]~=&;!#$%^(){}<>'

# Environment Variables for Common Paths
export WEB_CONENT="/usr/share/seclists/Discovery/Web-Content/"
export DNS="/usr/share/seclists/Discovery/DNS/"
export LEAK_DB="/usr/share/seclists/Passwords/Leaked-Databases"

# Environment Variables for Text Editors
export VISUAL=nano
export EDITOR=nano

# Environment Variable for PIP
export PIP_BREAK_SYSTEM_PACKAGES=1

# LS Colors Configuration
export LS_COLORS="$LS_COLORS:*.tar=1;31:*.tgz=1;31:*.arj=1;31:*.taz=1;31:*.lzh=1;31:*.lzma=1;31:*.tlz=1;31:*.txz=1;31:*.zip=1;31:*.z=1;31:*.Z=1;31:*.dz=1;31:*.gz=1;31:*.lz=1;31:*.xz=1;31:*.bz2=1;31:*.bz=1;31:*.tbz=1;31:*.tbz2=1;31:*.tz=1;31:*.deb=1;31:*.rpm=1;31:*.jar=1;31:*.war=1;31:*.ear=1;31:*.sar=1;31:*.rar=1;31:*.ace=1;31:*.zoo=1;31:*.cpio=1;31:*.7z=1;31:*.rz=1;31"

# Enviroment Variable for Micro LSP
MICRO_LSP=python=pyls,go=gopls,typescript=deno lsp={"importMap":"import_map.json"},rust=rust-analyzer
#----------Custom Functions----------#

# Set Tmux Target
set_target() {
    tmux set-environment target $1
}

# Get Tmux Target
get_target() {
    tmux show-environment target | awk -F '=' '{print $2}'
}

# Start a Simple HTTP Server
httpserver() {

    local port="${1:-8000}"
    local inets=$(ip addr | awk '/^[0-9]+: (.*):/ {print $2}')

    if [ -z ${2+x} ]; then
        if [[ $inets =~ tun[0-9]+ ||
            $inets =~ wlan[0-9]+  ||
            $inets =~ eth[0-9]+   ||
            $inets =~ enp.*[0-9]+ ]]; then
            inet=$MATCH
        else
          echo "[inet not found]"
        fi
    else
        inet=${2}
    fi
    local IP=$(ip -4 addr show "$inet" | grep -oP "(?<=inet\s)\d+(\.\d+){3}")
    echo "http://$IP:$port/"
    python -m http.server $port 1>/dev/null

}
