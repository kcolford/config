# ~/.bashrc

# colors
if [ "$TERM" != "dumb" ]; then
    RED="$(tput setaf 1)"
    GREEN="$(tput setaf 2)"
    RESET="$(tput sgr0)"
fi

# shell features
shopt -s cdspell checkwinsize dirspell globstar nullglob
HISTCONTROL=ignoredups

# modify prompt
PS1='\[$RED\]${?/#0/\[$GREEN\]}[\u@\h \W]\$ \[$RESET\]'

# useful variables
unitfile_regex='\.(service|socket|timer)$'

package() {
    case "$1" in
	-i)
	    shift
	    local groups="$(command pacman -Qge | cut -d ' ' -f 1 | sort -u)"
	    for group; do
		groups="$(fgrep -xv "$group" <<< "$groups")" 
	    done
	    echo "$groups"
	    echo
	    comm -3 <(pacman -Qqe | sort -u) <(pacman -Qqge $groups | sort -u)
	    ;;
	-R)
	    shift
	    sudo pacman -D --asdeps "$@"
	    while sudo pacman -R --noconfirm $(pacman -Qqdt); do
		:
	    done
	    ;;
	*)
	    if command -v pacaur > /dev/null 2>&1; then
		pacaur "$@"
	    else
		sudo pacman "$@"
	    fi
	    ;;
    esac
}

pb() {
    curl -F "c=@${1:--}" "https://ptpb.pw/?u=1"
}

# aliases
alias cmake='cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON'
alias cower='cower --rsort=votes'
alias df='df -h'
alias diff='diff -aur'
alias docker='sudo docker'
alias docker-compose='sudo docker-compose'
alias du='du -h'
alias e='$EDITOR'
alias emacs='emacs --no-splash'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias lns='ln -sfr'
alias ls='ls --color=auto -FC'
alias pacaur='pacaur --rsort=votes'
alias qrencode='qrencode -t ANSI'
alias synergys='synergys -a localhost'
alias tcpdump='sudo tcpdump -Z $USER'
alias xclip='xclip -selection clipboard'

