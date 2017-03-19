# ~/.profile

# some environment paths
export PREFIX="$HOME/.local"
export ANDROID_HOME="$HOME/android-sdk"
export GOPATH="$PREFIX"
export NPM_CONFIG_PREFIX="$PREFIX"
export PIP_USER=true

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH

# basic settings
export BROWSER="chromium"
export EDITOR="emacsclient -nw" ALTERNATE_EDITOR=""
