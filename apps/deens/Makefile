SOURCES = conf.ml conf_autogen.ml deens.ml
RESULT = deens

USE_MPL=	Yes
LIBS=		unix str mpl_stdlib getopt config
MELANGE_LIBS=	dns ounix config
DISTS=		getopt

include ../../mk/prog.mk

depend:: conf_autogen.ml
	@ :

conf_autogen.ml:
	$(MAKE) -f Makefile.gen_deens_ml
	./gen_deens_ml > $@

clean::
	$(MAKE) -f Makefile.gen_deens_ml clean
	rm -f conf_autogen.ml

