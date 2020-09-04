CACHE="/home/pd/.cache/common-lisp/sbcl-2.0.1.debian-linux-x64/home/pd/Develop/agador"
LISP=sbcl

default:
	@$(LISP) --eval "(asdf:make :agador)" --eval "(sb-ext:save-lisp-and-die \"agador.img\" :toplevel 'AGC:RUN)"
	@echo "Generated agador.img"

clean:
	@echo "Removing FASL files"
	@rm -fr ${CACHE}
	@rm -f agador.img
	@rm -f *.log
	@find . -name '*.*~' -delete

run:
	@$(LISP) --core agador.img --noinform

