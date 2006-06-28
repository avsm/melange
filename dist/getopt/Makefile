all: doc getopt.cmi getopt.cma
allopt: doc getopt.cmi getopt.cma getopt.cmxa

getopt.cmo: getopt.cmi getopt.ml
	ocamlc -c getopt.ml

getopt.cmi: getopt.mli
	ocamlc -c getopt.mli

getopt.cma: getopt.cmo
	ocamlc -o getopt.cma -a getopt.cmo

getopt.cmxa: getopt.cmx
	ocamlopt -o getopt.cmxa -a getopt.cmx

getopt.cmx: getopt.cmi getopt.ml
	ocamlopt -c getopt.ml

sample.cmo: getopt.cmi sample.ml
	ocamlc -c sample.ml

sample: getopt.cma sample.cmo
	ocamlc -o sample unix.cma getopt.cma sample.cmo

install:
	ocamlfind install getopt META getopt.cmi getopt.cma $(wildcard getopt.cmxa) $(wildcard getopt.o) $(wildcard getopt.a)

uninstall:
	ocamlfind remove getopt

.PHONY: doc
doc:
	mkdir -p doc
	ocamldoc -d doc -html getopt.mli

clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.o sample *~
	rm -Rf doc
