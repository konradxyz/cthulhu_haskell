all:
	ghc --make Testcthulhu.hs -o Testcthulhu
reload: distclean
	bnfc cthulhu.cf
	happy -gca -i Parcthulhu.y
	alex -g Lexcthulhu.x
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doccthulhu.ps
distclean: clean
	-rm -f Doccthulhu.* Lexcthulhu.* Parcthulhu.* Layoutcthulhu.* Skelcthulhu.* Printcthulhu.* Testcthulhu.* Abscthulhu.* Testcthulhu ErrM.* SharedString.* cthulhu.dtd XMLcthulhu.*

