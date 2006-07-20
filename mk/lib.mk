BUILD_ROOT=../..

all:: depend build

include $(BUILD_ROOT)/mk/base.mk

# implicit rule to convert mpl -> ml
%.ml : %.mpl
	$(MPLC) $(MPL_FLAGS) $< > $@

mpl_to_ml = $(call f_ml,$(MPL_AUTOGEN))
spl_to_spl0 = $(call f_spl,$(SPL_AUTOGEN0))
spl_to_ml0 = $(call f_mli,$(SPL_AUTOGEN0)) $(call f_ml,$(SPL_AUTOGEN0))
spl_sc0 = $(call f_ml,$(SPL_STATECALL0))

$(spl_to_ml0) $(spl_sc0): $(spl_to_spl0)
	$(SPLC) -t ocaml -s $(SPL_STATECALL0) -hdir $(SPL_DIR) -d false $(spl_to_spl0)
	
TRASH ?=
TRASH += $(spl_to_ml0) $(spl_sc0) $(mpl_to_ml)
SOURCES := $(mpl_to_ml) $(spl_sc0) $(spl_to_ml0) $(SOURCES)

include $(BUILD_ROOT)/config.mk

ifeq ($(BMODE),1)
	include $(BUILD_ROOT)/OCamlMakefile
endif

depend:: $(mpl_to_ml) $(spl_to_ml0)

build::
	$(MAKE) BMODE=1 ncl bcl

clean::
ifneq ($(BMODE),1)
	$(MAKE) BMODE=1 clean
endif
