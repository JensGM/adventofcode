a: a.hs
	stack ghc -- a.hs

test: a
	./a < test

run: a
	./a < input

clean:
	rm a.o
	rm a.hi
	rm a
