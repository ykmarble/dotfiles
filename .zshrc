# alias
alias あのあの='ping -c 3 www.google.co.jp'
alias ping='ping -c 3'
alias percol='percol --match-method migemo'
alias g='git'
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
alias -g G='|grep --color=auto'
alias -g L='|lv -c'
alias -g P='|percol'
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


#histry settings
HISTFILE=~/.histfile
HISTSIZE=10000000
SAVEHIST=10000000
setopt appendhistory
setopt extended_history

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
if [ $SSH_CONNECTION ];then
PROMPT='[%F{red}%n@%m%f]%# '
else
PROMPT='[%F{cyan}%n@%m%f]%# '
fi
RPROMPT='%F{green}[%d%f%1(v|%F{cyan}%1v%f|)%F{green}]%f'

#do ls after cd
function chpwd(){
    ls;
}

function mkdir(){
    /bin/mkdir $@ && cd $@;
}

# disable screen lock of Ctrl+S
stty stop undef

# for rbenv
eval "$(rbenv init -)"

# run tmux
which tmux 2>&1 >/dev/null && [ -z $TMUX ] && (tmux -2 attach || tmux -2 new-session)
