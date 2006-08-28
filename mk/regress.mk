BUILD_ROOT=../..

all:: depend build

include $(BUILD_ROOT)/mk/base.mk
include $(BUILD_ROOT)/config.mk

ifeq ($(BMODE),1)
	include $(BUILD_ROOT)/OCamlMakefile
endif

depend::

build::
	$(MAKE) BMODE=1 nc

clean::
ifneq ($(BMODE),1)
	$(MAKE) BMODE=1 clean
endif

regress:: build
	./$(RESULT) 
