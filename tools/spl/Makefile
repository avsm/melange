
# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = spl_utils.mli spl_utils.ml spl_location.ml spl_syntaxtree.ml \
	spl_parser.mli spl_lexer.ml spl_parser.ml spl_typechecker.mli \
	spl_typechecker.ml spl_cfg.mli spl_cfg.ml spl_optimiser.mli \
	spl_optimiser.ml spl_dot.ml spl_promela.ml spl_ocaml.ml \
	spl_debug.ml spl_gui.ml splc.ml
RESULT = splc
TRASH = spl_parser.output spl_parser.ml spl_lexer.ml
LIBS= str
depend:
all: nc dc

include $(OCAMLMAKEFILE)
