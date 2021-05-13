# color
autoload colors
colors

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 10000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

# vcs utils
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats ' (%s:%b)'
zstyle ':vcs_info:*' actionformats ' (%s*%b-%a)'

