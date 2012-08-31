# makefile to install my Emacs
all: clean compile install

# check out submodules, install vendor files, and compile JS2 mode
compile:
	-mkdir -p .tmp/desktops
	-mkdir -p .tmp/autosaves
	@git submodule sync
	@git submodule update --init
	@-cd vendor/distel && make
	@-cd vendor/magit && make
	@-cd vendor/js2-mode && make
	@-cd vendor/helm && make
	@-npm install

# make sure we're linked into the $USER Emacs sitefile
install:
	ln -s `pwd` ~/.emacs.d

# remove old directory and clean files
clean:
	rm -rf ~/.emacs.d
	git clean -qfxd
	find . -name "*.elc" -delete
