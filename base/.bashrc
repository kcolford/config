# ~/.bashrc

# colours
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

import() {
    for file; do
	if [[ -r "$file" ]]; then
	    # shellcheck disable=SC1090
	    source "$file"
	fi
    done
}

try_eval() {
    if command -v "$1" > /dev/null; then
	eval "$("$@")"
    else
	pkgfile -bv "$1"
    fi
}

import /usr/share/git/git-prompt.sh
PS1="\$(__git_ps1 \"(%s) \")$PS1"

# colourize prompt according to command status
PS1="\\[$RED\\]\${?/#0/\\[$GREEN\\]}$PS1\\[$RESET\\]"

# default options
alias curl='curl --location --cookie ~/.cookies.txt --cookie-jar ~/.cookies.txt'
alias diff='diff --text --unified --recursive'
alias ghc='ghc -dynamic'
alias gpg='gpg --armour'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias ls='ls --hide="*~" --color=auto --classify --dereference-command-line --human-readable'
alias qemu-system-x86_64='qemu-system-x86_64 -accel kvm -smp 2 -m 2048'
alias tcpdump='sudo tcpdump --relinquish-privileges $USER'

alias e='${EDITOR:-nano}'
alias la='ls -a'
alias ll='ls -l'
alias l='ls -la'
alias lr='ls -R'
alias lar='ls -AR'
alias sl='ls'
alias LS='ls'
alias igrep='grep --ignore-case'
alias fu='fuck'

# wrapper to enable relative paths for encfs
encfs() {
    if [[ $# != 2 ]] || [[ "$1" = -* ]] || [[ "$2" = -* ]]; then
	command encfs "$@"
    else
	command encfs "$(realpath "$1")" "$(realpath "$2")"
    fi
}

import /{etc,usr{,/local}/share/bash-completion}/bash_completion
import /usr/share/doc/pkgfile/command-not-found.bash
try_eval direnv hook bash &
try_eval hub alias -s
try_eval thefuck --alias &
disown -a
