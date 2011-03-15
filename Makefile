# makefile to install my Emacs
install:
	git submodule init
	git submodule update
	ln -s `pwd` ~/.emacs.d
	cd ~/.emacs.d
	emacs -q --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

clean:
	rm -rf *.elc
	git clean -f
