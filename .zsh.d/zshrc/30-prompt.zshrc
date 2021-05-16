# prompt
if [ $SSH_CONNECTION ] || [ -e "/.dockerenv" ];then
PROMPT='
%F{magenta}@%F{red}%m%F{white} > %F{green}%d%f%1(v|%F{blue}%1v%f|) $([ -n "${VIRTUAL_ENV}" ] && echo -n "%F{blue}(venv:" && echo -n "${VIRTUAL_ENV}" | sed -E "s%/.venv$%%" | sed -E "s%^/.+/%%" | tr -d "\n" && echo -n ")%f")
%# '
else
PROMPT='
%F{magenta}@%m%F{white} > %F{green}%d%f%1(v|%F{blue}%1v%f|) $([ -n "${VIRTUAL_ENV}" ] && echo -n "%F{blue}(venv:" && echo -n "${VIRTUAL_ENV}" | sed -E "s%/.venv$%%" | sed -E "s%^/.+/%%" | tr -d "\n" && echo -n ")%f")
%# '
fi
