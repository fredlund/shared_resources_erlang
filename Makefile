vpath %.erl src examples testing/src

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst examples/%.erl,ebin/%.beam,$(wildcard examples/*.erl))
BEAMS += $(patsubst testing/src/%.erl,ebin/%.beam,$(wildcard testing/src/*.erl))
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
	rm -f ebin/*.beam 


