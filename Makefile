all: cthulhu tester

SHELL:=/bin/bash
install:
	mkdir tmp
	mkdir -p ~/.cthulhu
	echo -n ${PWD} > ~/.cthulhu/dir
cthulhu: generator/src/cthulhu.hs generator/src/*/*.hs
	ghc --make -hidir tmp -odir tmp  -igenerator/src:generator/src/parser:generator/src/ast:generator/src/cmd:generator/src/seq:generator/src/par:generator/src/config -tmpdirtmp generator/src/cthulhu.hs -o cthulhu
tester: test/tester.hs
	ghc --make -hidir tmp -odir tmp -tmpdirtmp test/tester.hs -o tester
parser:
	diff <(bnfc --version)  <(echo 2.8)
	cd generator/src/parser; bnfc -m cthulhu.cf; make
clean:
	rm -f *.hi *.o cthulhu tmp/*.hi tmp/*.o run run_icpc.tar.gz tester
	cd generator/src/parser; make distclean
