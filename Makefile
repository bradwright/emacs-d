# makefile to install my Emacs
all: clean compile install

# check out submodules, install vendor files, and compile JS2 mode
compile:
	git submodule init
	git submodule update
	-cd vendor/distel && make
	-cd vendor/js2-mode && ~/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -f batch-byte-compile js2-mode.el
	-cd vendor/helm && ~/Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch -L . -f batch-byte-compile helm-config.el helm-match-plugin.el helm.el

# make sure we're linked into the $USER Emacs sitefile
install:
	ln -s `pwd` ~/.emacs.d

# remove old directory and clean files
clean:
	rm -f ~/.emacs.d
	git clean -qfxd
	find . -name "*.elc" -delete
