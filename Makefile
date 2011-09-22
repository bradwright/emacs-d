# makefile to install my Emacs
all: clean install

# check out submodules, install vendor files, and compile JS2 mode
install:
	git submodule init
	git submodule update
	cd vendor/magit && make
	ln -s `pwd` ~/.emacs.d
	emacs -q --batch --eval '(byte-recompile-directory "~/.emacs.d/vendor/js2-mode" 0)'

# remove old directory and clean files
clean:
	rm -f ~/.emacs.d
	git clean -qfxd
	find . -name "*.elc" -delete
