# ~/.bash_functions -*- mode:sh -*-

# shellcheck disable=SC2034
unitfile_regex='\.(service|socket|timer)$'

alert() {
    "$@"
    local ret=$?
    notify-send 'Terminal command finished' "$*"
    return $ret
}

listpkgs() {
    local pkg_grps="base base-devel"
    # shellcheck disable=SC2086
    comm -1 <(pacman -Qqet | sort -u) <(comm -13 <(pacman -Qqeg $pkg_grps | sort -u) <(pacman -Qqett | sort -u))
    echo
    pacman -Qqdtt | sort -u
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
    xargs npm install -g < ~/config/npm
    cabal update
    xargs cabal install < ~/config/cabal
    xargs gem install < ~/config/gem
    sudo pacman -Syu
}

if [ "$INSIDE_EMACS" ]; then
    man() {
	emacsclient -e "(man \"$*\")"
    }
    info() {
	emacsclient -e "(info \"$*\")"
    }
fi
