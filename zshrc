HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify
unsetopt prompt_cr prompt_sp
bindkey -e

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit
alias vim="nvim"
alias ls="ls -l"
alias ls="ls -l"
alias nrepl="clj -R:nrepl -m nrepl.cmdline"
alias cider="clj -A:cider"
alias lrun="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m"
alias leinrun="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
