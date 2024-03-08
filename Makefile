CC=ghc

all: flp-fun

flp-fun: flp-fun.hs
	$(CC) -o flp-fun flp-fun.hs

clean:
	rm -f flp-fun *.o *.hi