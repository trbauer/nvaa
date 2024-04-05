INSTALL_DST=c:/bin/

main:
	@echo make install
	@echo make nva
	@echo make collect_samples.exe  collect_opcodes.exe
	@echo make fptest

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

nva: nva.exe
nva.exe: Makefile src/Main.hs src/NVT/*.hs src/NVT/Encoders/*.hs src/NVT/Parsers/*.hs
	mkdir -p build/nva
	ghc  -hidir build/nva -odir build/nva --make src/Main.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

# If this fails, copy the command line and try direct.
install2: nva2
	@cp nva.exe ${INSTALL_DST}
nva2:
	mkdir -p build/nva2
	ghc  -hidir build/nva2 -odir build/nva2 --make src/Main.hs -O2 -o nva.exe -with-rtsopts="-N" -rtsopts -isrc -threaded ${PACKAGES}
	strip nva.exe

install: nva.exe
	@cp nva.exe ${INSTALL_DST}

collect_samples.exe: Makefile tools/CollectSamples.hs
	mkdir -p build/csel
	ghc  -hidir build/csel -odir build/csel --make tools/CollectSamples.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

collect_opcodes.exe: Makefile tools/CollectOpcodes.hs
	mkdir -p build/cops
	ghc  -hidir build/cops -odir build/cops --make tools/CollectOpcodes.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

fptest: fptest.exe
fptest.exe: Makefile tools/FPTest.hs src/NVT/Floats.hs src/NVT/Bits.hs
	mkdir -p build/libs
	ghc  -hidir build/fptst -odir build/fptst --make tools/FPTest.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded


run-micros:
	make -f tools/Makefile


clean:
	rm -rf build *.exe

