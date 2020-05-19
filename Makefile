CACHE="/home/pd/.cache/common-lisp/sbcl-1.4.5.debian-linux-x64/home/pd/Develop/agador"
LISP=sbcl

default:
	$(LISP) --eval "(asdf:make :agador)" --eval "(sb-ext:save-lisp-and-die \"agador.img\" :toplevel 'AGC:RUN)"

clean:
	@echo "Removing FASL files"
	@rm -fr ${CACHE}
	@rm -f agador.img

run:
	$(LISP) --core agador.img --noinform

dfa:
	@rm -f data/english.voca
	@cd data
	$(LISP) --eval "(asdf:make :agador)" --eval "(ags:make-voca \"data/english\")" --eval "(quit)"
	mkdfa english

