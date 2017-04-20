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
chroot() {
    for d in dev proc sys tmp; do
	sudo mount --rbind /$d "$1"/$d
    done
    command sudo chroot "$1" /bin/su -
    for d in dev proc sys tmp; do
	sudo umount -R "$1"/$d
    done
}
installbusybox() {
    install ~/Downloads/busybox-x86_64 ~/.local/bin/busybox
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
    local url="https://www.archlinux.org/mirrorlist"
    curl -s "$url/?country=${1:-CA}" | sed s/^#// | rankmirrors - | tee ~/scratch/mirrorlist
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
setup_home() {
    xdg-user-dirs-update
    mkdir -p ~/{projects,scratch}
    chattr -R -f +C "$XDG_DOWNLOAD_DIR" "$PREFIX"/share/
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

# some final setup
setup_home
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
