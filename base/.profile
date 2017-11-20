export NAME="${NAME:-$(getent passwd "$USER" | cut -d : -f 5 | cut -d , -f 1)}"
export EMAIL="${EMAIL:-${USER}@$(hostname --domain)}"

export PAGER=less
export LESS=FRSXi
export EDITOR="emacsclient -nw"
export TEXEDIT="$EDITOR +%d %s"
export ALTERNATE_EDITOR="nano"

export PREFIX="$HOME"/.local
. ~/.push_env

# fail gracefully without systemd
install -Dm755 "$(which true)" "$XDG_RUNTIME_DIR"/fake_bin/systemctl
PATH="$PATH:$XDG_RUNTIME_DIR"/fake_bin systemctl --user import-environment PATH
rm -r "$XDG_RUNTIME_DIR"/fake_bin
