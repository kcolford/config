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

# personal commands
alias aria2c='aria2c -c -d ~/Downloads --bt-seed-unverified'
alias cp='cp --reflink=auto'
alias diff='diff -aur'
alias e='$EDITOR'
alias ls='ls --color=auto -FC'
alias systemctl='systemctl --user'
alias run='systemd-run --user '
alert() {
    "$@"
    local ret=$?
    notify-send 'Terminal command finished' "$*"
    return $ret
}
configure() {
    if [ -x ./configure ]; then
	./configure --prefix="$PREFIX" "$@"
    elif [ -f ./CMakeLists.txt ] && which cmake; then
	cmake -DCMAKE_INSTALL_PREFIX="$PREFIX" "$@"
    else
	false
    fi
}
ensurepip() {
    python -m ensurepip --user --default-pip
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
	"$@" > "$cache" &
	disown
    else
	"$@" > "$cache"
    fi
    # shellcheck disable=SC1090
    . "$cache"
}
mirrorlist() {
    local url=https://www.archlinux.org/mirrorlist
    curl -s "$url"/?country="${1:-CA}" | sed s/^#// | rankmirrors - | tee ~/scratch/mirrorlist
}
pb() {
    curl -F "c=@${1:--}" https://ptpb.pw/?u=1
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
subcommand() {
    for i; do
	# shellcheck disable=SC2139
	alias "$i"="$i "
    done
}
unitfiles() {
    egrep '\.(service|socket|timer)$'
}

# some final setup
setup_home
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
subcommand alert nohup
