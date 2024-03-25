CC=ghc
CFLAGS=-O2 -Wall
all: flp-fun

flp-fun: flp-fun.hs
	$(CC) $(CFLAGS) -o flp-fun flp-fun.hs

clean:
	rm -f flp-fun *.o *.hi