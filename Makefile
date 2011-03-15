# makefile to install my Emacs
all: clean install

install:
	git submodule init
	git submodule update
	ln -s `pwd` ~/.emacs.d
	emacs -q --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

clean:
	# unlink the directory
	rm ~/.emacs.d
	# remove old compiled files
	rm -rf *.elc
