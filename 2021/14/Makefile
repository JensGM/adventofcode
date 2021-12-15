a: a.hs
	stack ghc -- a.hs

test: a
	./a < test

test2: a
	./a < test2

test3: a
	./a < test3

run: a
	./a < input

clean:
	rm a.o
	rm a.hi
	rm a
