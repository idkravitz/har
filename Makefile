.PHONY: all clean

HC = ghc
HC_OPTS = --make -O2 -cpp $(EXTRA_HC_OPTS)
EXTRA_HC_OPTS = -fno-cse

SRC = huffman.hs archivecommon.hs rle.hs optparse.hs har.hs  
OBJS = huffman.o archivecommon.o rle.o optparse.o har.o

all: har

har: $(SRC) 
	ghc -o $@ $(SRC) $(HC_OPTS)

clean:
	rm -f har *.o *.hi
