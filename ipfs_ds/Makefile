BIN=beams
LIB=lib

BEAMS=\
    ipfs.beam ipfs_dht.beam ipfs_helper.beam

all: $(BEAMS)

%.beam: %.erl
	erlc -I $(LIB) $^ 

clean:
	rm -f $(BEAMS)
	rm -f *.dump
	rm -f *.pyc
