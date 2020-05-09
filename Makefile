CACHE="/home/pd/.cache/common-lisp/sbcl-1.4.5.debian-linux-x64/home/pd/Develop/agador"

clean:
	@echo "Removing FASL files"
	@rm -fr ${CACHE}

run:
	lisp --core agador

default:
	lisp --eval '(asdf:make :agador)'

