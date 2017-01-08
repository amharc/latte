all: stack build lib/runtime.o

stack:
	wget https://www.stackage.org/stack/linux-x86_64-static -O stack.tgz
	tar xvvf stack.tgz
	mv stack-*/stack stack
	rm -rf stack.tgz stack-*

build:
	PATH="/home/students/inf/PUBLIC/MRJP/ghc-7.10.2/bin:$(PATH)" ./stack build
	PATH="/home/students/inf/PUBLIC/MRJP/ghc-7.10.2/bin:$(PATH)" ./stack install --local-bin-path="$(shell pwd)"

lib/runtime.o: lib
	gcc src/runtimec/runtime.c -c -o lib/runtime.o -O3

lib:
	mkdir lib

.PHONY: build
