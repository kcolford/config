# ~/.profile

# some environment paths
export ANDROID_HOME="$HOME/android-sdk"
export GOPATH="$HOME/.local"
export NPM_CONFIG_PREFIX="$HOME/.local"
export PIP_USER=true
export PREFIX="$HOME/.local"

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH

# improve compilation time
MAKEFLAGS="-j$(nproc)" && export MAKEFLAGS
[ -f ~/.config/modprobed.db ] && export LSMOD="$HOME"/.config/modprobed.db

# basic settings
export BROWSER="chromium"
export EDITOR="editor"
export TEXEDIT="editor +%d %s"
