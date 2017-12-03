# ~/.bashrc

# colors
if [ "$TERM" != "dumb" ]; then
    RED="$(tput setaf 1)"
    # shellcheck disable=SC2034
    GREEN="$(tput setaf 2)"
    RESET="$(tput sgr0)"
fi

# shell features
shopt -s checkwinsize globstar
HISTCONTROL=erasedups

# modify prompt
PS1="\[$RED\]\${?/#0/\[$GREEN\]}$PS1\[$RESET\]"

# aliases
alias cower='cower --rsort=votes'
alias curl='curl -Ls'
alias diff='diff -aur'
alias e='$EDITOR'
alias gpgv='gpg --verify'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias lns='ln -sfr'
alias ls='ls --color=auto -CFH'
alias xclip='xclip -selection clipboard'

for file in /{etc,usr{,/local}/share/bash-completion}/bash_completion; do
    if [[ -f "$file" ]]; then
	. "$file"
    fi
done
