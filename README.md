Personal Config
===============

This is just a collection of personal config files or _dotfiles_. I
manage them using GNU Stow and that's the only dependency. Just go to
your home folder and run

	git clone https://github.com/kcolford/config
	cd config
	ln -s ~ .homelink
	xargs stow < normal.lst

Installing Stow
---------------

Installing stow is relatively simple so long as Perl is installed.
Just download from <https://ftp.gnu.org/gnu/stow/stow-latest.tar.gz>
and use `make` to build it.
