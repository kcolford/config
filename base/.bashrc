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

# colourize prompt according to command status
PS1="\\[$RED\\]\${?/#0/\\[$GREEN\\]}$PS1\\[$RESET\\]"

# default options
alias curl='curl --location --cookie ~/.cookies.txt --cookie-jar ~/.cookies.txt'
alias diff='diff --text --unified --recursive'
alias ghc='ghc -dynamic'
alias gpg='gpg --armor'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep --ignore-case'
alias ls='ls --hide="*~" --color=auto --classify --dereference-command-line --human-readable'
alias qemu-system-x86_64='qemu-system-x86_64 -accel kvm -smp 2 -m 2048'
alias tcpdump='sudo tcpdump --relinquish-privileges $USER'

alias e='${VISUAL:-${EDITOR:-nano}}'

alias la='ls -a'
alias ll='ls -l'
alias l='ls -la'
alias lr='ls -R'
alias lar='ls -AR'
alias sl='ls'
alias LS='ls'

# github integration for git
git() {
    if command -v hub > /dev/null 2>&1; then
	hub "$@"
    else
	command git "$@"
    fi
}

# wrapper to enable relative paths for encfs
encfs() {
    if [[ $# != 2 ]] || [[ "$1" = -* ]] || [[ "$2" = -* ]]; then
	command encfs "$@"
    else
	command encfs "$(realpath "$1")" "$(realpath "$2")"
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

if command -v direnv > /dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi
