# makefile to install my Emacs
all: clean install

# check out submodules, install vendor files, and compile JS2 mode
install:
	git submodule init
	git submodule update
	-cd vendor/distel && make
	ln -s `pwd` ~/.emacs.d

# remove old directory and clean files
clean:
	rm -f ~/.emacs.d
	git clean -qfxd
	find . -name "*.elc" -delete
