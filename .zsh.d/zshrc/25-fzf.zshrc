# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS="--height 40%"

# fzf+cdr
function select_cdr(){
    local selected_dir=$(cdr -l | awk '{ print $2 }' | fzf)
      #fzf --preview 'f() { sh -c "ls -hFGl $1" }; f {}')
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N select_cdr
bindkey '^T' select_cdr
