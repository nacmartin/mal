ERLC=erlc
ERLCFLAGS=-o
SRCDIR=src
BEAMDIR=./ebin

all: 
	mkdir -p $(BEAMDIR) ;
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
clean: 
	rm -rf $(BEAMDIR) ;
	rm -rf erl_crush.dump
