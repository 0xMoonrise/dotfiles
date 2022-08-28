# Lines configured by zsh-newuser-install

export HISTSIZE=SAVEHIST=2000
setopt sharehistory
setopt extendedhistory
export HISTFILE="~/.zsh_history"

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

export TERM=xterm-256color

plugins=(
    # other plugins...
    zsh-autosuggestions
)
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/moonrise/.zshrc'


autoload -U compinit
compinit
# End of lines added by compinstall
autoload -U colors && colors

zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots)

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
