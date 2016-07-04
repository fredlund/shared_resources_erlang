COMPILER=erlc

vpath %.erl src src/schedulers

SPECSOURCES = $(wildcard src/specs/*.erl)

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst src/schedulers/%.erl,ebin/%.beam,$(wildcard src/schedulers/*.erl))
SPECBEAMS = $(patsubst src/specs/%.erl,ebin/%.beam,$(wildcard src/specs/*.erl))

HEADERS = $(wildcard src/*.hrl)
EFLAGS = +debug_info 

all: ebin specs main
	(cd examples; make)

main: ${BEAMS}

ebin:
	mkdir -p ebin

specs: $(SPECBEAMS)

$(SPECBEAMS): $(SPECSOURCES)
	$(COMPILER) -pa ebin $(EFLAGS) -o ebin $(SPECSOURCES)

ebin/%.beam: %.erl $(HEADERS) $(SPECBEAMS)
	$(COMPILER) -pa ebin $(EFLAGS) -o ebin $<

dialyzer: ebin main
	dialyzer ebin/*beam examples/*/ebin

docs:
	make edoc

clean:
	rm -f ebin/*.beam 	
	(cd examples; make clean)



