all:
	ghc -threaded -fllvm -O5 -rtsopts -with-rtsopts="-K100000000" --make HeuRika
	strip HeuRika

prof:
	ghc -threaded -fllvm -O5 -rtsopts -with-rtsopts="-K100000000" --make HeuRika
	ghc -threaded -fllvm -O5 -rtsopts -prof -caf-all -auto-all -osuf .p_o -with-rtsopts="-K100000000" --make HeuRika

clean:
	find . -iname \*.hi -exec rm -f {} \;
	find . -iname \*.o -exec rm -f {} \;
	find . -iname \*.p_o -exec rm -f {} \;
	rm -f HeuRika
