CACHE="/home/pd/.cache/common-lisp/sbcl-1.4.5.debian-linux-x64/home/pd/Develop/agador"

default:
	sbcl --eval "(asdf:make :agador)" --eval "(sb-ext:save-lisp-and-die \"agador.img\" :toplevel 'AGC:RUN)"

clean:
	@echo "Removing FASL files"
	@rm -fr ${CACHE}
	@rm -f agador.img

run:
	sbcl --core agador.img


