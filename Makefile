.PHONY: all clean

HC = ghc
HC_OPTS = --make -O2 -cpp $(EXTRA_HC_OPTS)

SRC = optparse.hs har.hs  
OBJS = optparse.o har.o

all: har

har: $(SRC) 
	ghc -o $@ $(SRC) $(HC_OPTS)

clean:
	rm -f har *.o *.hi
