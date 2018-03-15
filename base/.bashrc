# ~/.bashrc

# colors
if [ "$TERM" != "dumb" ]; then
    RED="$(tput setaf 1)"
    GREEN="$(tput setaf 2)"
    RESET="$(tput sgr0)"
fi

# shell features
shopt -s checkwinsize globstar
HISTCONTROL=ignoreboth

# modify prompt
PS1="\\[$RED\\]\${?/#0/\\[$GREEN\\]}$PS1\\[$RESET\\]"

# aliases
alias cower='cower --rsort=votes'
alias curl='curl --location --cookie ~/.cookies.txt --cookie-jar ~/.cookies.txt'
alias diff='diff --text --unified --recursive'
alias e='${VISUAL:-${EDITOR:-vi}}'
alias ghc='ghc -dynamic'
alias gpg='gpg --armor'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep --ignore-case'
alias ls='ls --color=auto --classify --dereference-command-line --human-readable'
alias pacaur='pacaur --rsort=votes'
alias qemu-system-x86_64='qemu-system-x86_64 -accel kvm -smp 2 -m 2048'
alias tcpdump='sudo tcpdump --relinquish-privileges $USER'
alias xclip='xclip -selection clipboard'

alias la='ls -a'
alias ll='ls -l'
alias l='ls -la'

alias sl='ls'
alias LS='ls'

git() {
    if command -v hub > /dev/null 2>&1; then
	hub "$@"
    else
	command git "$@"
    fi
}

for file in /{etc,usr{,/local}/share/bash-completion}/bash_completion; do
    if [[ -f "$file" ]]; then
	# shellcheck disable=SC1090
	. "$file"
    fi
done
