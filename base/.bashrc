# ~/.bashrc

# colors
if [ "$TERM" != "dumb" ]; then
    RED="$(tput setaf 1)"
    GREEN="$(tput setaf 2)"
    RESET="$(tput sgr0)"
fi

# shell features
shopt -s autocd
shopt -s cdspell
shopt -s checkhash
shopt -s checkwinsize
shopt -s cmdhist
shopt -s direxpand
shopt -s dirspell
shopt -s globstar
shopt -s histappend
shopt -s mailwarn
shopt -s no_empty_cmd_completion
HISTCONTROL=ignoreboth

# modify prompt
PS1="\\[$RED\\]\${?/#0/\\[$GREEN\\]}$PS1\\[$RESET\\]"

# aliases
alias curl='curl --location --cookie ~/.cookies.txt --cookie-jar ~/.cookies.txt'
alias diff='diff --text --unified --recursive'
alias e='${VISUAL:-${EDITOR:-nano}}'
alias ghc='ghc -dynamic'
alias gpg='gpg --armor'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep --ignore-case'
alias ls='ls --hide="*~" --color=auto --classify --dereference-command-line --human-readable'
alias qemu-system-x86_64='qemu-system-x86_64 -accel kvm -smp 2 -m 2048'
alias tcpdump='sudo tcpdump --relinquish-privileges $USER'

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

if [[ -r /usr/share/doc/pkgfile/command-not-found.bash ]]; then
    . /usr/share/doc/pkgfile/command-not-found.bash
fi
