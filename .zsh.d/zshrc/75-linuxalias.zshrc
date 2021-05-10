if [ "x$(uname)" = "xLinux" ]; then
    alias open='xdg-open'
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -o -selection clipboard'
fi
