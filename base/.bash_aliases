# ~/.bash_aliases -*- mode:sh -*-

alias aria2c='aria2c ${XDG_DOWNLOAD_DIR:+--dir "$XDG_DOWNLOAD_DIR"}'
alias cp='cp --reflink=auto'
alias curl='curl -sL'
alias diff='diff -aur'
alias docker='sudo docker'
alias docker-compose='sudo -E docker-compose'
alias e='emacsclient'
alias emacs='emacs --no-splash'
alias emacsclient='emacsclient -s $EMACS_SERVER'
alias gpg='gpg'
alias gpgv='gpg --verify'
alias ls='ls --color=auto -FC'
alias make='make -j$(nproc)'
alias mkfs='mkfs -t btrfs'
alias mount='sudo mount'
alias qrencode='qrencode -t ANSI'
if [ "$UID" = 0 ]; then
    alias sudo='env '
else
    alias sudo='sudo '
fi
alias tarx='tar -x -C ~/scratch/ -f'
alias umount='sudo umount'
alias unzip='unzip -d ~/scratch/'
