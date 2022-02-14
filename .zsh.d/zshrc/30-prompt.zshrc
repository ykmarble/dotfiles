# vcs
zstyle ':vcs_info:*' formats '%F{blue}%b%f %F{green}%c%f%F{yellow}%u%f%F{yellow}%m%f'
zstyle ':vcs_info:*' actionformats '%F{blue}%b%f %F{red}*%a%f'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "*"
zstyle ':vcs_info:git:*' unstagedstr "*"

# prompt
if [ $SSH_CONNECTION ] || [ -e "/.dockerenv" ];then
PROMPT='
%F{magenta}@%F{red}%m%F{white} %F{green}%~%f ${vcs_info_msg_0_} $([ -n "${VIRTUAL_ENV}" ] && echo -n "%F{blue}(venv:" && echo -n "${VIRTUAL_ENV}" | sed -E "s%/.venv$%%" | sed -E "s%^/.+/%%" | tr -d "\n" && echo -n ")%f")
%# '
else
PROMPT='
%F{magenta}@%m%F{white} %F{green}%~%f ${vcs_info_msg_0_} $([ -n "${VIRTUAL_ENV}" ] && echo -n "%F{blue}(venv:" && echo -n "${VIRTUAL_ENV}" | sed -E "s%/.venv$%%" | sed -E "s%^/.+/%%" | tr -d "\n" && echo -n ")%f")
%# '
fi
