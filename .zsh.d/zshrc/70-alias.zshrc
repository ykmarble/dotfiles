# alias
alias a='cd ..'
alias dc='cd'
alias g='git'
alias l='ls'
alias ls='ls -hFN --color=auto'
alias la='ls -A'
alias ll='ls -l'
alias lla='ls -Al'
if [[ $(command -v exa) ]]; then
    alias ls='exa --icons'
    alias la='exa -a --icons'
    alias ll='exa -l --git --icons --time-style long-iso'
    alias lla='exa -a -l --git --icons --time-style long-iso'
    alias lt='exa -l -T -L 2 --git --git-ignore --icons --time-style long-iso'
    alias lta='exa -a -l -T -L 2 --git --icons --time-style long-iso'
fi
alias lv='lv -Ou8'
alias diff='colordiff -u'
alias view="vim -R -M"
alias -g G='|grep --color=auto'
alias -g L='|lv -c'
