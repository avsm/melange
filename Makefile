depend:
	cd lib && ${MAKE} depend
	cd tools && ${MAKE} depend

all:
	cd lib && ${MAKE}
	cd tools && ${MAKE}

clean:
	cd lib && ${MAKE} clean
	cd tools && ${MAKE} clean

install:
	@echo Not done yet, sorry!
