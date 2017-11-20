# ~/.bashrc

# colors
if [ "$TERM" != "dumb" ]; then
    RED="$(tput setaf 1)"
    # shellcheck disable=SC2034
    GREEN="$(tput setaf 2)"
    RESET="$(tput sgr0)"
fi

# shell features
shopt -s checkwinsize globstar
HISTCONTROL=erasedups

# modify prompt
PS1="\[$RED\]\${?/#0/\[$GREEN\]}$PS1\[$RESET\]"

# aliases
alias cower='cower --rsort=votes'
alias diff='diff -aur'
alias docker='sudo docker'
alias docker-compose='sudo docker-compose'
alias e='$EDITOR'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias lns='ln -sfr'
alias ls='ls --color=auto -FC'
alias tcpdump='sudo tcpdump -Z $USER'
alias xclip='xclip -selection clipboard'
