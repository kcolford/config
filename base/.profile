export NAME="${NAME:-$(getent passwd "$USER" | cut -d : -f 5 | cut -d , -f 1)}"
export EMAIL="${EMAIL:-${USER}@$(hostname --domain)}"

export PAGER=less
export LESS=FRSXi
export EDITOR="emacsclient -nw"
export TEXEDIT="$EDITOR +%d %s"
export ALTERNATE_EDITOR="nano"

export PREFIX="$HOME/.local"
export CPATH="$PREFIX/include${CPATH:+:$CPATH}"
export GOPATH="$PREFIX/go${GOPATH:+:$GOPATH}"
export LD_LIBRARY_PATH="$PREFIX/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
export LIBRARY_PATH="$PREFIX/lib${LIBRARY_PATH:+:$LIBRARY_PATH}"
export PATH="$HOME/bin:$PREFIX/bin:/usr/lib/ccache/bin:${PATH:+:$PATH}"
export GOBIN="$PREFIX/bin"
export NPM_CONFIG_PREFIX="$PREFIX"
export PYTHONUSERBASE="$PREFIX"
