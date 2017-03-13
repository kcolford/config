# ~/.bashrc

# don't do anything on a non-interactive terminal
[[ $- = *i* ]] || return

# setup the home directory making sure that pip is installed
mkdir -p ~/{docs/personal,junk,projects,scratch}
chattr +C ~/junk

# personal aliases
alias cp='cp --reflink=auto'
alias ls='ls --color=auto -F'

# terminal specific features
case "$TERM" in
    dumb)
	export PAGER=cat
	;;
    *)
	RED="\[$(tput setaf 1)\]"
	GREEN="\[$(tput setaf 2)\]"
	RESET="\[$(tput sgr0)\]"
	;;
esac

# colourize prompt according to exit code of last command
PS1="$RED\${?/#0/$GREEN}$PS1$RESET"

# roundabout way to load things from commands without a delay
cache=~/.cache/bash
mkdir -p $cache
load() {
    if [ -f $cache/"$*" ]; then
	"$@" > $cache/"$*" & disown
    else
	"$@" > $cache/"$*"
    fi
    . $cache/"$*"
}
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
unset cache load
