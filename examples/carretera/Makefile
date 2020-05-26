COMPILER=erlc

vpath %.erl src 

BEAMS = $(patsubst %.erl,ebin/%.beam,$(wildcard *.erl))

HEADERS = $(wildcard *.hrl)
EFLAGS = +debug_info 

all: ebin main

main: ${BEAMS}

ebin:
	mkdir -p ebin

specs: $(SPECBEAMS)

$(SPECBEAMS): $(SPECSOURCES)
	$(COMPILER) -pa ebin $(EFLAGS) -o ebin $(SPECSOURCES)

ebin/%.beam: %.erl $(HEADERS) $(SPECBEAMS)
	$(COMPILER) -I ../../src -pa ../../ebin -pa ebin $(EFLAGS) -o ebin $<

dialyzer: ebin main
	dialyzer ebin/*beam examples/*/ebin

docs:
	make edoc

clean:
	rm -f ebin/*.beam 	



