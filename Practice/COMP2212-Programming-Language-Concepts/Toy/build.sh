#!/usr/bin/bash

./generate.sh && \
	cabal build && \
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.3/Toy-0.1.0.0/x/Toy/build/Toy/Toy ./
