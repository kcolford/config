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
shopt -s globstar

# colourize prompt according to exit code of last command
PS1="$RED\${?/#0/$GREEN}$PS1$RESET"

# import basic environment
. ~/.environment

# personal commands
alias aria2c='mkdir -p ~/junk && aria2c -c -d ~/junk --bt-seed-unverified'
alias alert='alert '
alias cp='cp --reflink=auto'
alias diff='diff -aur'
alias e='$EDITOR'
alias ls='ls --color=auto -FC'
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
    comm -13 <(pacman -Qqeg $pkg_grps | sort -u) <(pacman -Qqe | sort -u)
}
load() {
    local cache=~/.cache/bash
    mkdir -p $cache
    if [ -f $cache/"$*" ]; then
	"$@" > $cache/"$*" & disown
    else
	"$@" > $cache/"$*"
    fi
    . $cache/"$*"
}
mirrorlist() {
    local url=https://www.archlinux.org/mirrorlist
    curl -s "$url"/?country="${1:-CA}" | sed s/^#// | rankmirrors - | tee ~/scratch/mirrorlist
}
pb() {
    curl -F "c=@${1:--}" https://ptpb.pw/?u=1
}
reload() {
    . ~/.bashrc
}
setup_home() {
    mkdir -p ~/{docs/personal,junk,projects,scratch}
    chattr +C ~/junk
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
