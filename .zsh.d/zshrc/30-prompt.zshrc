# prompt
if [ $SSH_CONNECTION ] || [ -e "/.dockerenv" ];then
PROMPT='
%F{magenta}@%F{red}%m%F{white} > %F{green}%d%f%1(v|%F{blue}%1v%f|)%F{magenta}%f
%# '
else
PROMPT='
%F{magenta}@%m%F{white} > %F{green}%d%f%1(v|%F{blue}%1v%f|)%F{magenta}%f
%# '
fi
