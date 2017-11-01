# ~/.bashrc

set -k
. $ENV
set +k

# terminal specific features
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
RESET="$(tput sgr0)"

# shell features
shopt -s cdspell checkwinsize dirspell dotglob globstar
HISTCONTROL=ignoredups

# modify prompt
PROMPT_COMMAND=''
trap 'PROMPT_COLOUR="$GREEN";SECONDS=0' DEBUG
trap 'PROMPT_COLOUR="$RED"' ERR
PS1='\[$PROMPT_COLOUR\]${?/#0}[${SECONDS}s \u@\h \W]\$ \[$RESET\]'

# useful variables
unitfile_regex='\.(service|socket|timer)$'

mirrorlist() {
    curl -s "https://www.archlinux.org/mirrorlist/?country=${1:-CA}" | sed s/^#// | rankmirrors - | tee ~/scratch/mirrorlist
}

pb() {
    curl -F "c=@${1:--}" "https://ptpb.pw/?u=1"
}

reload() {
    . ~/.bashrc
}

# completions
if [ -r /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -r /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# aliases
alias config='git -C ~/config'
eval "$(complete -p git | sed 's/git$/config/')"
alias df='df -h'
alias diff='diff -aur'
alias docker='sudo docker'
alias docker-compose='sudo docker-compose'
alias du='du -h'
alias e='$EDITOR'
alias emacs='emacs --no-splash'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias ls='ls --color=auto -FC'
alias make='make -j$(nproc)'
alias pacaur='pacaur --rsort votes'
alias qrencode='qrencode -t ANSI'
alias rsync='rsync -a'
alias sudo='sudo '
alias tcpdump='sudo tcpdump -Z $USER'
alias xclip='xclip -selection clipboard'
