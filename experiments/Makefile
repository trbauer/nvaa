PACKAGES=\
	-package ansi-terminal \
	-package array \
	-package bytestring \
	-package containers \
	-package directory \
	-package filepath \
	-package mtl \
	-package parsec \
	-package process \
	-package transformers

bexp.exe: Build.hs
	mkdir -p build/bexp
	ghc -hidir build/bexp -odir build/bexp \
		--make Build.hs -O2 -o $@ -with-rtsopts="-N" \
		-rtsopts -threaded ${PACKAGES}
	strip $@