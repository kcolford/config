# ~/.profile

# who am i
export NAME="git config user.name"
export EMAIL="git config user.email"

# system profile
. /etc/profile

# installation paths
export PREFIX="$HOME/local"
export NPM_CONFIG_PREFIX="$HOME/npm"
export PIP_USER=true

# path
PATH="$HOME/.cabal/bin:$PATH"	# cabal
rubyuserdir="$(ruby -e 'puts Gem.user_dir')" && PATH="$rubyuserdir/bin:$PATH" # ruby
PATH="$HOME/perl5/bin:$PATH"	    # perl
PATH="$HOME/npm/bin:$PATH"	    # npm
PATH="$HOME/.local/bin:$PATH"	    # python
PATH="$HOME/go/bin:$PATH"	    # go
PATH="$HOME/local/bin:$PATH"	    # personal
export PATH

# cpan
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

# basic settings
export BROWSER="chromium"
export ALTERNATE_EDITOR="" EDITOR="${EDITOR:-emacsclient}"
export TEXEDIT="${TEXEDIT:-$EDITOR +%d %s}"
export LESS=FRSXi
export DIFFPROG="diff -aur"
export PAGER=less

# passwords
if ! grep -q '^default' ~/.netrc; then
    touch ~/.netrc
    chmod 600 ~/.netrc
    echo default login anonymous password password >> ~/.netrc
fi

# btrfs
for i in ~/.config/chromium ~/.cache/chromium ~/junk ~/.mu ~/.ipfs ~/.cache/aria2; do
    mkdir -p "$i"
    chattr +C "$i"
done
