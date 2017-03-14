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

# colourize prompt according to exit code of last command
PS1="$RED\${?/#0/$GREEN}$PS1$RESET"

# personal commands
alias cp='cp --reflink=auto'
alias e='editor'
alias ls='ls --color=auto -F'
prefix() {
    echo "$HOME"/local/stow/"$(basename "$PWD")"
}
configure() {
    ./configure --prefix="$(prefix)" "$@"
}
listpkgs() {
    comm -13 <(pacman -Qqg base base-devel | sort -u) <(pacman -Qqe | sort -u)
}
ensurepip() {
    python -m ensurepip --user --default-pip
}
mirrorlist() {
    local url=https://www.archlinux.org/mirrorlist
    curl $url/?country="${1:-CA}" | sed s/^#// | rankmirrors -
}
reload() {
    . ~/.bashrc
}
pb() {
    curl -F "c=@${1:--}" https://ptpb.pw/?u=1
}
setup_home() {
    mkdir -p ~/{docs/personal,junk,local/stow,projects,scratch}
    chattr +C ~/junk
    touch ~/local/stow/.stow
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
unitfiles() {
    egrep '\.(service|socket|timer)$'
}

# some final setup
setup_home
load hub alias -s
load npm completion
load pip completion --bash
load thefuck --alias
