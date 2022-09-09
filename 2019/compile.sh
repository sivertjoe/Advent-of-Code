#!/bin/zsh

IN="\"main.lisp\""
OUT="\"main\""
cd $1
sbcl --script <(echo -e "(load $IN)\n(sb-ext:save-lisp-and-die $OUT :executable t :toplevel 'main)")
