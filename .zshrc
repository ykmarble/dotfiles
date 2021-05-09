for file in .zsh.d/zshrc/*; do
    [ -f "$file" ] || continue
    #echo loading "$file"
    source "$file"
done
