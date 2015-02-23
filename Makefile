COMPILER=erlc

vpath %.erl src examples schedulers examples examples/readers-writers examples/robots examples/multibuffer testing/src qa

BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst schedulers/%.erl,ebin/%.beam,$(wildcard schedulers/*.erl))
BEAMS += $(patsubst examples/%.erl,ebin/%.beam,$(wildcard examples/*.erl))
BEAMS += $(patsubst examples/multibuffer/%.erl,ebin/%.beam,$(wildcard examples/multibuffer/*.erl))
BEAMS += $(patsubst examples/readers-writers/%.erl,ebin/%.beam,$(wildcard examples/readers-writers/*.erl))
BEAMS += $(patsubst examples/robots/%.erl,ebin/%.beam,$(wildcard examples/robots/*.erl))
BEAMS += $(patsubst testing/src/%.erl,ebin/%.beam,$(wildcard testing/src/*.erl))
EFLAGS = +debug_info 
BEAMS += $(patsubst qa/%.erl,ebin/%.beam,$(wildcard qa/*.erl))
EFLAGS = +debug_info 

all: ebin main

main: ${BEAMS}

ebin:
	mkdir -p ebin

ebin/%.beam: %.erl
	$(COMPILER) -pa ebin $(EFLAGS) -o ebin $<

dialyzer: ebin main
	dialyzer ebin/*beam

docs:
	make edoc

clean:
	rm -f ebin/*.beam 


