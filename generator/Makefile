all:
	ghc --make -hidir tmp -odir tmp  -isrc:src/parser:src/ast:src/cmd:src/seq:src/par -tmpdirtmp src/cthulhu.hs -o cthulhu
clean:
	rm -f *.hi *.o cthulhu tmp/*.hi tmp/*.o
