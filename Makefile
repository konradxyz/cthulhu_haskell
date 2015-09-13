all: cthulhu tester

install:
	mkdir tmp
	mkdir -p ~/.cthulhu
	echo -n ${PWD} > ~/.cthulhu/dir
cthulhu: generator/src/cthulhu.hs generator/src/*/*.hs
	ghc --make -hidir tmp -odir tmp  -igenerator/src:generator/src/parser:generator/src/ast:generator/src/cmd:generator/src/seq:generator/src/par -tmpdirtmp generator/src/cthulhu.hs -o cthulhu
tester: test/tester.hs
	ghc --make -hidir tmp -odir tmp -tmpdirtmp test/tester.hs -o tester
clean:
	rm -f *.hi *.o cthulhu tmp/*.hi tmp/*.o run run_icpc.tar.gz tester
