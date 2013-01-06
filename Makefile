TARGET	:= $(HOME)/.emacs.d
EMACS   := emacs

.PHONY: all compile install clean_dir clean

# makefile to install my Emacs
all: clean compile install

# check out submodules, install vendor files, and compile JS2 mode
compile:
	-mkdir -p .tmp/desktops
	-mkdir -p .tmp/autosaves
	@git submodule sync
	@git submodule update --init
	@-npm install

# make sure we're linked into the $USER Emacs sitefile
install: clean_dir
	ln -sf $(CURDIR) $(TARGET)

clean_dir:
	rm -rf $(TARGET)

# remove old directory and clean files
clean: clean_dir
	find . -name "*.elc" -delete

clean_all: clean
	git clean -qfxd

clean_packages:
	git clean -dfx ./packages
