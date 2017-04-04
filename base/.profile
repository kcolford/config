# ~/.profile

# setup misc environment variables
# shellcheck disable=SC1090
. ~/.environment

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH
