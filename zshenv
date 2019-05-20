export GO111MODULE=on
export GOROOT="$HOME/bin/go"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export PATH=".:$HOME/bin:$HOME/Dropbox/bin:$HOME/Dropbox/go/bin:$GOROOT/bin:$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/.cargo/bin:$PATH"
export TERM=xterm-256color
export EDITOR="nvim"
export PAGER=less
export LANG=en_US.UTF-8
export LC_CTYPE=fi_FI.UTF-8
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
export MOZ_USE_XINPUT2=1
export PROMPT="
%F{green}%d%f%B
 $%b "
