# alias
alias あのあの='ping -c 3 www.google.co.jp'
alias ping='ping -c 3'
alias percol='percol --match-method migemo'
alias a='cd ..'
alias g='cd $(ls -A | peco)'
alias c='pygmentize -O style=vim -f console256 -g'
alias w='wpa_cli'
alias z='cd'
alias l='ls'
alias e='emacs'
alias o='open'
alias ls='ls -hF --color=auto'
alias la='ls -A'
alias ll='ls -l'
alias lla='ls -Al'
alias sshe='cocot -t UTF-8 -p EUC-JP -- ssh'
alias lv='lv -Ou8'
alias python='python2'
alias uplatex='uplatex -interaction nonstopmode -halt-on-error -file-line-error'
alias scala='scala -Xlint'
alias -g G='|grep --color=auto'
alias -g L='|lv -c'
alias -g P='|peco'
alias -g C='|column'
alias strace='strace -s 2000 -f -tt'
alias -s tex=platex
alias -s pdf=open
alias -s java=javac
function doclass() {
    java $(basename $1 .class);
}
alias -s class=doclass
function extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.xz) tar Jxvf $1;;
    *.zip) unzip $1;;
    *.lzh) lha e $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.tar.Z) tar zxvf $1;;
    *.gz) gzip -dc $1;;
    *.bz2) bzip2 -dc $1;;
    *.Z) uncompress $1;;
    *.tar) tar xvf $1;;
    *.arj) unarj $1;;
  esac
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

case "${OSTYPE}" in
    darwin*)
        source $HOME/.zsh.d/mac
        ;;
    linux*)
        source $HOME/.zsh.d/linux
        ;;
esac

export PATH="$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.gem/ruby/2.2.0/bin "

#histry settings
HISTFILE=~/.histfile
HISTSIZE=10000000
SAVEHIST=10000000
setopt appendhistory
setopt extended_history
setopt hist_ignore_dups

# ignore C-d
setopt IGNOREEOF

#color settings
autoload colors
colors
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

#ignore duplications in history
setopt hist_ignore_dups

#ignore command which start with space in history
setopt hist_ignore_space

#update history instantly
setopt inc_append_history

setopt prompt_subst
unsetopt beep

#push directry stack after cd
setopt auto_pushd
setopt pushd_ignore_dups

#change current directory without "cd" command
setopt autocd

#correct command
setopt correct

#enable to use powerful glob,like regular expression
setopt extendedglob
setopt braceccl

#change keybind like emacs
bindkey -e

#expand environment variant in prompt
setopt prompt_subst

#initialize completion
autoload -Uz compinit
compinit

#use menu style completion
zstyle ':completion:*:default' menu select=2

#use colored competion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

#ignore char difference which is lowercase or uppercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[._-]=*'

#allow completion to use cache
zstyle ':completion:*' use-cache yes

#set separator
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true

#change apperance of completion
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer _expand _complete _match _prefix _list
zstyle ':completion:*:messages' format '%F{yellow}%d%f'$DEFAULT
zstyle ':completion:*:warnings' format '%F{red}No matches.%f'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:descriptions' format '%F{yellow}%U%d%u%f'$DEFAULT


#use completion after "="
setopt magic_equal_subst

#complete dot-file without typeing "."
setopt globdots

#complete PSID after "jobs"
setopt long_list_jobs

#think "/" as word-divide unit
WORDCHARS=${WORDCHARS:s,/,,}

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

#git utils
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats ' (%s:%b)'
zstyle ':vcs_info:*' actionformats ' (%s*%b-%a)'

precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    psvar[1]="$vcs_info_msg_0_"
}

#prompt settings
PROMPT='%F{cyan}✲ﾟ｡.(✿╹◡╹)ﾉ☆.｡₀:*ﾟ✲ﾟ*:₀｡%f%# '
if [ $SSH_CONNECTION ];then
RPROMPT='%F{green}[%f%F{red}%m%f:%F{green}%d%f%1(v|%F{cyan}%1v%f|)%F{green}]%f'
else
RPROMPT='%F{green}[%f%F{green}%m%f:%F{green}%d%f%1(v|%F{cyan}%1v%f|)%F{green}]%f'
fi


#do ls after cd
function chpwd(){
    ls;
}

function mkdir(){
    /bin/mkdir $@ && cd $@;
}

function dict(){
    w3m "http://ejje.weblio.jp/sentence/content/$1"
}

# disable screen lock of Ctrl+S
stty stop undef

# zaw -- zsh anything.el-like widget
#source ${HOME}/.zsh.d/zaw/zaw.zsh
#zstyle ':filter-select' case-insensitive yes # 絞り込みをcase-insensitiveに
#bindkey '^xb' zaw-tmux-window
#bindkey '^xh' zaw-history
#bindkey '^x^f' zaw-cdr
#bindkey '^xr' zaw-ssh-hosts
#bindkey '^xi' zaw-open-file
