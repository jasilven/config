HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify prompt_subst
unsetopt prompt_cr prompt_sp BEEP
bindkey -e
# set tilde to work
set -o magicequalsubst
autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

git_branch() {
    git_status="$(git status 2> /dev/null)"
    pattern="On branch ([^[:space:]]*)"
    if [[ ! ${git_status} =~ "(working (tree|directory) clean)" ]]; then
        state="*"
    fi
    if [[ ${git_status} =~ ${pattern} ]]; then
      branch=${match[1]}
      branch_cut=${branch:0:35}
      if (( ${#branch} > ${#branch_cut} )); then
          echo "${branch_cut}…${state}"
      else
          echo "${branch}${state}"
      fi
    fi
}

bindkey -v
bindkey 'jk' vi-cmd-mode
bindkey -M vicmd -- 'gh' beginning-of-line
bindkey -M vicmd -- 'gl' end-of-line
bindkey -M vicmd -- 'dl' kill-line

PS1+='${VIMODE}'
#   '$' for normal insert mode
#   a big red 'I' for command mode - to me this is 'NOT insert' because red
function zle-line-init zle-keymap-select {
    DOLLAR='%B%F{green}$%b'
    GIANT_N='%B%F{red}N%b'
    VIMODE="${${KEYMAP/vicmd/$GIANT_N}/(main|viins)/$DOLLAR}"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

PROMPT='
%F{blue}%d%f%F{yellow}  $(git_branch)  %F{cyan}%(1j.(jobs!).)
%F{green%}%B ${VIMODE} %F{none}%b'

alias vim="nvim"
alias nrepl="clj -R:nrepl -m nrepl.cmdline"
alias cider="clj -A:cider"
alias lrun="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m"
alias leinrun="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m"
alias ls="exa"
alias ll="exa -l"
# alias clj="clj -J-Dclojure.server.jvm=\"{:port 5555 :accept clojure.core.server/io-prepl}\""
alias find="fd"
alias tm="tmux attach || tmux new"
alias mutt=neomutt
alias open=xdg-open
alias gnome="dbus-run-session gnome-session"
alias more=less
