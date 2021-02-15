#!/usr/bin/bash

#
# Use this script to (re)generate lexer and parser from the Tokens.x and
# Grammar.y files using `alex` and `happy`.
#
# Make sure you have appropriate versions of both `alex` and `happy`!
#

alex Tokens.x
happy Grammar.y
