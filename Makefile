vpath %.erl src examples

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst examples/%.erl,ebin/%.beam,$(wildcard examples/*.erl))
EFLAGS = +debug_info 

all: ebin main

main: ${BEAMS}

ebin:
	mkdir -p ebin

ebin/%.beam: %.erl
	erlc -pa ebin $(EFLAGS) -o ebin $<

dialyzer: ebin main
	dialyzer ebin/*beam

docs:
	make edoc

clean:
	rm -f ebin/*.beam examples/ebin/*.beam


