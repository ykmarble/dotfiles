#environment settings
typeset -U path PATH
export PATH=$PATH:$HOME/bin
export EDITOR=vim
export LANG="ja_JP.UTF-8"
export KCODE=u
export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8
export GOPATH=$HOME/Sources/Go
export PATH=$PATH:$GOPATH/bin

export DefaultIMModule=fcitx
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
