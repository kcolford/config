# ~/.profile

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/go/bin:$PATH"
PATH="$HOME/node_modules/.bin:$PATH"
rubyuserdir="$(ruby -e 'Gem.user_dir')" && PATH="$PATH:$rubyuserdir/bin"
export PATH="$HOME/bin:$PATH"

# perform user installs for python
export PIP_USER=true

# setup android
export ANDROID_HOME="$HOME/android/android-sdk"
mkdir -p "$ANDROID_HOME"

# improve compilation time
MAKEFLAGS="-j$(nproc)" && export MAKEFLAGS
[ -f ~/.config/modprobed.db ] && export LSMOD="$HOME"/.config/modprobed.db

# basic settings
export BROWSER="chromium"
export EDITOR="e"
export TEXEDIT="e +%d %s"
