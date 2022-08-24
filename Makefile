SHELL = /bin/sh

all: sexp transpile

tangle-solparser:
	make tangle -f ./parsers/solidity

build-solparser:
	sbcl --load "./parsers/solidity/exec.lisp"


tangle-rustgen:
	make tangle -f ./generators/rust

build-rustgen:
	sbcl --load "./generators/rust/exec-transpiler.lisp"


build-transpiler:
	sbcl --load "./exec-transpiler.lisp"

sexp: mep.sol solparser
	./solparser -p ./mep.sol -o mep.sexp

transpile: mep.sexp transpiler
	./transpiler -t mep.sexp


clean:
	rm ./solparser
	rm ./transpiler
	rm ./mep.sexp
