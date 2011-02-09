.PHONY: all clean

all: har

har: optparse.hs har.hs 
	ghc --make -O2 $^ -o har

clean:
	rm -f har *.o *.hi
