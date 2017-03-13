# ~/.profile

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/go/bin:$PATH"
PATH="$HOME/.nodejs/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
export PATH="$HOME/bin:$PATH"

# perform user installs
export PIP_USER=true NPM_CONFIG_PREFIX="$HOME/.nodejs"

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
