CACHE="/home/pd/.cache/common-lisp/sbcl-2.0.1.debian-linux-x64/home/pd/Develop/agador"
LISP=sbcl --noinform

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
	@rm -f agador.log
	@$(LISP) --core agador.img

text:
	@rm -f agador.log
	@$(LISP) --core agador.img --corpus t.txt --count 20

prepare:
	@echo "Loading packages"
	@$(LISP) --eval "(ql:quickload :CL-UTILITIES)" --eval "(quit)"
	@$(LISP) --eval "(ql:quickload :LMDB)" --eval "(quit)"
	@$(LISP) --eval "(ql:quickload :CL-PPCRE)" --eval "(quit)"
	@$(LISP) --eval "(ql:quickload :LOG4CL)" --eval "(quit)"
	@$(LISP) --eval "(ql:quickload :CL-DATE-TIME-PARSER)" --eval "(quit)"
	@$(LISP) --eval "(ql:quickload :ALEXANDRIA)" --eval "(quit)"

