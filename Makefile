SHELL = /bin/sh

all: tangle build

tangle:
	make tangle -f ./solipsism/Makefile

build:
	make build -f  ./solipsism/Makefile

demo:
	./solipsism/solcheck -p ./mep.sol
