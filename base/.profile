# ~/.profile

# setup misc environment variables
# shellcheck disable=SC1090
. ~/.environment

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
PATH="$GOPATH/bin:$PATH"
PATH="$PREFIX/bin:$PATH"
export PATH

# to be run before everything else but not all the time
[ "$SSH_AUTH_SOCK" ] || eval "$(ssh-agent -s)"
mkdir -p ~/junk ~/projects ~/scratch
chattr -R -f +C ~/junk "$PREFIX"/share/
which pip > /dev/null 2>&1 || python -m ensurepip --user --default-pip
