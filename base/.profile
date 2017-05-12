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

# cpan
PATH="/home/kieran/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/kieran/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/kieran/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/kieran/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/kieran/perl5"; export PERL_MM_OPT;


# to be run before everything else but not all the time
[ "$SSH_AUTH_SOCK" ] || eval "$(ssh-agent -s)"
mkdir -p ~/junk ~/projects ~/scratch
chattr -R -f +C ~/junk "$PREFIX"/share/
which pip > /dev/null 2>&1 || python -m ensurepip --user --default-pip
