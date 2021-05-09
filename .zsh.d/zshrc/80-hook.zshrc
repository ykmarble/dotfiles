# hooks
function my_preexec(){
    # update title
    echo -n "\033]2;"
    echo -n "${1}"
    if [ ${SSH_CONNECTION} ]; then
        echo -n "@${HOST}"
    fi
    echo -n "\007"

    # record last command
    LASTCMD="${1}"
}

function my_precmd () {
    laststatus=$? # preserve code
    # update title
    echo -n "\033]2;"
    if [ ${laststatus} -eq 0 ]; then
        echo -n "✔"
    else
        echo -n "🔥"
    fi
    echo -n " zsh"
    if [ ${SSH_CONNECTION} ]; then
        echo -n "@${HOST}"
    fi
    echo -n "\007"
    # vcs
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    psvar[1]="$vcs_info_msg_0_"
}

function my_chpwd(){
    ls;
}

add-zsh-hook preexec my_preexec
add-zsh-hook precmd my_precmd
add-zsh-hook chpwd my_chpwd
