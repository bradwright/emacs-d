# makefile to install my Emacs
emacs:=   ~/Applications/Emacs.app/Contents/MacOS/Emacs

all: clean compile install

# check out submodules, install vendor files, and compile JS2 mode
compile:
	-mkdir -p .tmp/desktops
	@git submodule sync
	@git submodule update --init
	@-cd vendor/distel && make
	-cd vendor/js2-mode && $(emacs) -Q --batch -f batch-byte-compile js2-mode.el
	@npm install

# make sure we're linked into the $USER Emacs sitefile
install:
	ln -s `pwd` ~/.emacs.d

# remove old directory and clean files
clean:
	rm -f ~/.emacs.d
	git clean -qfxd
	find . -name "*.elc" -delete
