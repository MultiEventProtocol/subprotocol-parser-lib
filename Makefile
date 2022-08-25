SHELL = /bin/sh

all: sexp transpile

# SolParser
solparser: tangle-solparser build-solparser


tangle-solparser:
	make tangle -f ./parsers/solidity/Makefile

build-solparser:
	sbcl --load "./parsers/solidity/exec.lisp"


# RustGen
transpiler: build-rustgen


tangle-rustgen:
	make tangle -f ./generators/rust/Makefile

build-rustgen:
	sbcl --load "./exec-transpiler.lisp"

sexp: mep.sol solparser
	./solparser -p ./mep.sol -o mep.sexp

transpile: mep.sexp transpiler
	./transpiler -t mep.sexp


clean:
	rm ./solparser
	rm ./transpiler
	rm ./mep.sexp
