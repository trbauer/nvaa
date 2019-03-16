main:
	@echo make libsamples.exe
	@echo make nva.exe

libsamples.exe:
	mkdir -p build/libs
	ghc  -hidir build/libs -odir build/libs --make tools/CollectSamples.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded

nva.exe: src/Main.hs
	mkdir -p build/nva
	ghc  -hidir build/nva -odir build/nva --make src/Main.hs -O2 -o $@ -with-rtsopts="-N" -rtsopts -isrc -threaded


clean:
	rm -rf build *.exe