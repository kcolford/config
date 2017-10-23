# ~/.bashrc

# don't do anything on a non-interactive terminal
[[ $- = *i* ]] || return

# universal stuff
. ~/.env

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

# shell features
shopt -s cdspell checkwinsize dirspell dotglob globstar
HISTCONTROL=ignoredups

# colourize prompt according to exit code of last command
PS1="$RED\${?/#0/$GREEN}$PS1$RESET"

# useful variables
unitfile_regex='\.(service|socket|timer)$'

# functions
alert() {
    "$@"
    local ret=$?
    notify-send 'Terminal command finished' "$*"
    return $ret
}
emacsify() {
    if [ "$EMACS_SERVER" ] && [ "$#" = 2 ]; then
	emacsclient -s "$EMACS_SERVER" -e "($1 \"$2\")"
    else
	"$@"
    fi
}
listpkgs() {
    local pkg_grps="base base-devel"
    comm -1 <(pacman -Qqet | sort -u) <(comm -13 <(pacman -Qqeg $pkg_grps | sort -u) <(pacman -Qqett | sort -u))
    echo
    pacman -Qqdtt | sort -u
}
load() {
    local cache="${XDG_CONFIG_CACHE:-$HOME/.cache}"/bash/"$(echo "$*" | base64)"
    mkdir -p "$(dirname "$cache")"
    if [ -f "$cache" ]; then
	(nice "$@" > "$cache" || rm "$cache") &
	disown
    else
	"$@" > "$cache" || rm "$cache"
    fi
    . "$cache"
}
alias load='load '
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

# aliases
alias cp='cp -a --reflink=auto'
alias curl='curl -sL'
alias df='df -h'
alias diff='diff -aur'
alias docker='sudo docker'
alias docker-compose='sudo -E docker-compose'
alias e='$EDITOR'
alias emacs='emacs --no-splash'
alias gpgv='gpg --verify'
alias igrep='grep -i'
alias ls='ls --color=auto -FC'
alias make='make -j$(nproc)'
alias mkfs='mkfs -t btrfs'
alias pacaur='pacaur --rsort votes'
alias qrencode='qrencode -t ANSI'
alias rsync='rsync -a'
alias sudo='sudo '
alias tarx='tar -x -C ~/scratch/ -f'
alias tcpdump='sudo tcpdump -Z $USER'
alias xclip='xclip -selection clipboard'
#alias unzip='unzip -d ~/scratch/'
# for shells in emacs
alias info='emacsify info'
alias man='emacsify man'
which hub > /dev/null && alias git='hub'

# completions
# load curl https://raw.githubusercontent.com/ipfs/go-ipfs/master/misc/completion/ipfs-completion.bash
# load curl https://raw.githubusercontent.com/docker/compose/master/contrib/completion/bash/docker-compose
# load hub alias -s
# load npm completion
# load pip completion --bash
# _bitcoin-cli() {
#     COMPREPLY=( $(compgen -W "$(bitcoin-cli help |& sed '/^=/d;/^$/d;s/\([a-zA-Z0-9]\+\).*/\1/')" -- "${COMP_WORDS[COMP_CWORD]}") )
# }
# complete -F _bitcoin-cli bitcoin-cli
