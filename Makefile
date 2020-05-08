
clean:
	@echo "Removing FASL files"
	@rm -fr ~/.cache/common-lisp

run:
	sbcl --load main.lisp

default:
	lisp --eval '(asdf:make :agador)'

