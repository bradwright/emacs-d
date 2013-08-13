TARGET	:= $(HOME)/.emacs.d
EMACS   := emacs

.PHONY: all compile install clean_dir clean venv

# makefile to install my Emacs
all: clean compile install venv

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

venv: requirements.txt
	test -d venv || virtualenv --no-site-packages venv
	./venv/bin/easy_install readline
	./venv/bin/pip install -Ur requirements.txt
	touch venv/bin/active

clean_dir:
	rm -rf $(TARGET)

clean_venv:
	rm -rf venv

# remove old directory and clean files
clean: clean_dir clean_venv
	find . -name "*.elc" -delete

clean_all: clean
	git clean -qfxd

clean_packages:
	git clean -dfx ./packages
