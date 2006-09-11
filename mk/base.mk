USE_MPL ?= No
MPL_FLAGS ?=

ifneq ($(MPL_AUTOGEN),"")
	USE_MPL := Yes
endif

USE_SPL ?= No
ifneq ($(SPL_AUTOGEN),"")
	USE_SPL := Yes
endif

SPL_DIR=$(BUILD_ROOT)/tools/spl
MPL_DIR=$(BUILD_ROOT)/tools/mpl
MPLC=$(MPL_DIR)/mplc
SPLC=$(SPL_DIR)/splc

OCAMLMAKEFILE=$(BUILD_ROOT)/OCamlMakefile

$(MPLC):
	$(MAKE) -C $(BUILD_ROOT)/tools/mpl all

$(SPLC):
	$(MAKE) -C $(BUILD_ROOT)/tools/spl all

tool_dep =
	
ifeq ($(USE_MPL),Yes)
 LIBDIRS += $(BUILD_ROOT)/tools/mpl
 INCDIRS += $(BUILD_ROOT)/tools/mpl
 LIBS += mpl_stdlib
 tool_dep += $(MPLC)
endif

ifeq ($(USE_SPL),Yes)
 LIBDIRS += $(BUILD_ROOT)/tools/spl
 INCDIRS += $(BUILD_ROOT)/tools/spl
 LIBS += spl_stdlib
 tool_dep += $(SPLC)
endif

depend:: $(tool_dep)

extra_mel_libs= $(patsubst %,$(BUILD_ROOT)/lib/%,$(MELANGE_LIBS))
extra_mel_libs+=$(patsubst %,$(BUILD_ROOT)/dist/%,$(DISTS))
LIBS+= $(MELANGE_LIBS)
LIBDIRS+= $(extra_mel_libs)
INCDIRS+= $(extra_mel_libs)

depend::
	@set -e; for i in $(MELANGE_LIBS); do $(MAKE) -C $(BUILD_ROOT)/lib/$$i depend all; done
	@set -e; for i in $(DISTS); do $(MAKE) -C $(BUILD_ROOT)/dist/$$i -f Makefile.wrapper all; done

f_spl = $(call patsubst,%,%.spl,$1)
f_mpl = $(call patsubst,%,%.mpl,$1)
f_ml =  $(call patsubst,%,%.ml,$1)
f_mli =  $(call patsubst,%,%.mli,$1)
