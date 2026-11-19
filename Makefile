prog: main.hs Automata.hs
	ghc -O2 -o prog main.hs

clean:
	rm -f prog *.o *.hi

.PHONY: clean
