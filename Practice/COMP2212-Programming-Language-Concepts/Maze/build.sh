#!/usr/bin/bash

#
# Use this script to build executable parser from all the *.hs files using the
# Glasgow Haskell Compiler Tool (GHC). Make sure you have GHC installed!
#
# This script relies on the `generate.sh` in this directory -- do not remove it.
#

./generate.sh
ghc Main.hs -o maze
rm *.o *.hi # cleanup
