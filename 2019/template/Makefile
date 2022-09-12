SHELL :=/bin/bash

all:
	@sbcl --script <(echo -e "(load \"main.lisp\")\n(main)")
test: FORCE
	@sbcl --script <(echo -e "(load \"main.lisp\")\n(main)") "test"

FORCE: ;
