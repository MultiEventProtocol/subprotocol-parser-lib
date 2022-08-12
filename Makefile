SHELL = /bin/sh

all: demo

build:
	sbcl --load "./exec.lisp"
	echo ":builded"

demo:
	./solipsism/solcheck -p ./mep.sol
