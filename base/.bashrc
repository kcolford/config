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

# import basic environment
# shellcheck disable=SC1090
. ~/.environment

# import aliases
# shellcheck disable=SC1090
. ~/.bash_aliases

# useful stuff
# shellcheck disable=SC2034
unitfile_regex='\.(service|socket|timer)$'

# personal commands
alert() {
    "$@"
    local ret=$?
    notify-send 'Terminal command finished' "$*"
    return $ret
}
listpkgs() {
    local pkg_grps="base base-devel gnome"
    # shellcheck disable=SC2086
    comm -13 <(pacman -Qqeg $pkg_grps | sort -u) <(pacman -Qqe | sort -u)
}
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
mirrorlist() {
    curl -s "https://www.archlinux.org/mirrorlist/?country=${1:-CA}" | sed s/^#// | rankmirrors - | tee ~/scratch/mirrorlist
}
pb() {
    curl -F "c=@${1:--}" "https://ptpb.pw/?u=1"
}
pbs() {
    curl -F "c=${1:-@-}" "https://ptpb.pw/u?u=1"
}
reload() {
    # shellcheck disable=SC1090
    . ~/.bashrc
}
touch() {
    for file in "$@"; do
	case "$file" in
	    -*) ;;
	    */*) mkdir -p "$(dirname "$file")" ;;
	esac
    done
    command touch "$@"
}
update() {
    xargs pip install -U < ~/config/pip
    xargs go get -u < ~/config/go
    xargs npm update -g < ~/config/npm
    cabal update
    xargs cabal install < ~/config/cabal
    xargs gem install < ~/config/gem

    sudo pacman -Syu
}

# some final setup
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
