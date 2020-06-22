export LANG=C
export LC_CTYPE=de_DE.UTF-8
export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig
export FZF_DEFAULT_COMMAND="find ."
export PATH=$PATH:$(go env GOPATH)/bin
eval "$(rbenv init -)"
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
