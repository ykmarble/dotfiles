ls &>/dev/null
if [ $? != 0 ]; then
    alias ls='ls -hFG'
fi
