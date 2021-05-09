#initialize completion
autoload -Uz compinit
compinit

#use menu style completion
zstyle ':completion:*:default' menu select=2

#use colored competion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

#ignore char difference which is lowercase or uppercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'l:|=* r:|=*'

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

