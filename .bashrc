# ~/.bashrc

# If not running interactively, don't do anything bash specific
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
eval "$(thefuck -a)"
