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
if distcc -j > /dev/null 2>&1; then
    export CCACHE_PREFIX="distcc"
    export MAKEFLAGS="-j$(distcc -j)"
else
    export MAKEFLAGS="-j$(nproc)"
fi

export PREFIX_="$HOME"/.local
. ~/.push_env

systemctl --user import-environment
