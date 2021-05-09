# histroy settings
HISTFILE=~/.histfile
HISTSIZE=10000000
SAVEHIST=10000000
setopt appendhistory
setopt extended_history
setopt hist_ignore_dups

# ignore C-d
setopt IGNOREEOF

# ignore duplications in history
setopt hist_ignore_dups

# ignore command which start with space in history
setopt hist_ignore_space

# update history instantly
setopt inc_append_history

unsetopt beep

# push directry stack after cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus

# change current directory without "cd" command
setopt autocd

# correct command
setopt correct

# enable to use powerful glob,like regular expression
setopt extendedglob
setopt braceccl

# expand environment variant in prompt
setopt prompt_subst

# completetion ignoring suffix
setopt complete_in_word

# use completion after "="
setopt magic_equal_subst

# complete PSID after "jobs"
setopt long_list_jobs

# think "/" as word-divide unit
WORDCHARS=${WORDCHARS:s,/,,}

# disable screen lock of Ctrl+S
stty stop undef

# change keybind like emacs
bindkey -e

bindkey "^R" history-incremental-pattern-search-backward
bindkey "^S" history-incremental-pattern-search-forward
bindkey '^[h' backward-kill-word

