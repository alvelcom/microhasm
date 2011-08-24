# Simple haskell makefile

all: main.exe

main.exe: 
	ghc -XFlexibleInstances --make assembler.hs main.hs

clean:
	rm *.hi 
	rm *.o 
	rm *.exe

