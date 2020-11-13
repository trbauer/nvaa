INSTALL_DST=c:/bin/

main:
	@echo make install
	@echo make nva.exe
	@echo make collect_samples.exe|collect_opcodes.exe
	@echo make fptest

nva.exe: Makefile src/Main.hs src/NVT/*.hs src/NVT/Encoders/*.hs src/NVT/Parsers/*.hs
	mkdir -p build/nva
	ghc  -hidir build/nva -odir build/nva --make src/Main.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

install: nva.exe
	@cp nva.exe ${INSTALL_DST}

collect_samples.exe: Makefile tools/CollectSamples.hs
	mkdir -p build/libs
	ghc  -hidir build/libs -odir build/libs --make tools/CollectSamples.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

collect_opcodes.exe: Makefile tools/CollectOpcodes.hs
	mkdir -p build/libs
	ghc  -hidir build/libs -odir build/libs --make tools/CollectOpcodes.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded
	strip $@

fptest.exe: Makefile tools/FPTest.hs
	mkdir -p build/libs
	ghc  -hidir build/libs -odir build/libs --make tools/FPTest.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded


clean:
	rm -rf build *.exe