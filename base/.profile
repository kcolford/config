# ~/.profile

# setup path
PATH="/usr/lib/ccache/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/go/bin:$PATH"
PATH="$HOME/.nodejs/bin:$PATH"
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH"
PATH="$HOME/local/bin:$PATH"
PATH="$HOME/bin:$PATH"
export PATH

# perform user installs
export PIP_USER=true NPM_CONFIG_PREFIX="$HOME/.nodejs"

# setup android
export ANDROID_HOME="$HOME/local/stow/android-sdk"

# improve compilation time
MAKEFLAGS="-j$(nproc)" && export MAKEFLAGS
[ -f ~/.config/modprobed.db ] && export LSMOD="$HOME"/.config/modprobed.db

# basic settings
export BROWSER="chromium"
export EDITOR="editor"
export TEXEDIT="editor +%d %s"
