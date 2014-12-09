vpath %.erl src

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
EFLAGS = +debug_info 

all: ebin main

main: ${BEAMS}

ebin:
	mkdir -p ebin

ebin/%.beam: %.erl
	erlc $(EFLAGS) -o ebin $<


dialyzer: ebin main
	dialyzer ebin/*beam

docs:
	make edoc

clean:
	rm -f ebin/*.beam examples/ebin/*.beam


