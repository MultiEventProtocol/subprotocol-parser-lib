SHELL = /bin/sh

all: demo

tangle:
	make tangle -f ./solipsism/Makefile

build:
	sbcl --load "./exec.lisp"

build-transpiler:
	sbcl --load "./exec-transpiler.lisp"

demo:
	./solparser -p ./mep.sol

sexp:
	./solparser -p ./mep.sol -o mep.sexp

transpile:
	./transpiler -t mep.sexp
