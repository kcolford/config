export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-$(mktemp -d)}"
export NAME="${NAME:-$(getent passwd "$USER" | cut -d : -f 5 | cut -d , -f 1)}"
export EMAIL="${EMAIL:-${USER}@$(hostname --domain)}"

for i in ~/.profile.d/*; do
    if [ -f "$i" ]; then
	. "$i"
    fi
done

export PAGER=less
export LESS=FRSXi
export EDITOR="emacsclient -nw"
export TEXEDIT="$EDITOR +%d %s"
export ALTERNATE_EDITOR="nano"

export PATH=/usr/lib/ccache/bin:"$PATH"
export CCACHE_PREFIX="distcc"
export MAKEFLAGS="-j$(distcc -j || nproc)"

export PREFIX_="$HOME"/.local
. ~/.push_env

# fail gracefully without systemd
install -Dm755 "$(which true)" "$XDG_RUNTIME_DIR"/fake_bin/systemctl
PATH="$PATH:$XDG_RUNTIME_DIR"/fake_bin systemctl --user import-environment PATH
rm -r "$XDG_RUNTIME_DIR"/fake_bin
