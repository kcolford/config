# ~/.bashrc

# don't do anything on a non-interactive terminal
[[ $- = *i* ]] || return

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

# enable some shell features
shopt -s autocd cdspell checkwinsize direxpand dirspell dotglob globstar nullglob

# colourize prompt according to exit code of last command
PS1="$RED\${?/#0/$GREEN}$PS1$RESET"

# import some stuff
# shellcheck disable=SC1090
[ -r ~/.environment ] && . ~/.environment
[ -r ~/.bash_aliases ] && . ~/.bash_aliases
[ -r ~/.bash_functions ] && . ~/.bash_functions

# load some stuff
load() {
    local cache="${XDG_CONFIG_CACHE:-$HOME/.cache}"/bash/"$*"
    mkdir -p "$(dirname "$cache")"
    if [ -f "$cache" ]; then
	("$@" > "$cache" || rm "$cache") &
	disown
    else
	"$@" > "$cache" || rm "$cache"
    fi
    # shellcheck disable=SC1090
    . "$cache"
}
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
unset load
