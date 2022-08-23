SHELL = /bin/sh

all: demo

build:
	sbcl --load "./exec.lisp"
	echo ":builded"

demo:
	./solparser -p ./mep.sol

sexp:
	./solparser -p ./mep.sol -o mep.sexp
