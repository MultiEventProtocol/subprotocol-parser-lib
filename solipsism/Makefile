SHELL = /bin/sh

all: tangle build

tangle:
	emacs -nw ./doc.org -l ./generator.el \
		--eval "(progn (org-babel-tangle) (kill-emacs))"
	echo ":tangled"
build:
	sbcl --load "./exec.lisp"
	echo ":builded"
