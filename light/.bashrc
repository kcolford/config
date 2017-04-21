# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias e='$EDITOR'
alias emacs='emacs --no-splash'
alias ls='ls --color=auto'

export EDITOR='emacs -Q'

reload() {
    . ~/.bashrc
}
