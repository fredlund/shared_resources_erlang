COMPILER=erlc

vpath %.erl 

BEAMS = $(patsubst %.erl,ebin/%.beam,$(wildcard *.erl))

HEADERS = $(wildcard *.hrl)
EFLAGS = +debug_info 

all: ebin main

main: ${BEAMS}

ebin:
	mkdir -p ebin

ebin/%.beam: %.erl $(HEADERS)
	$(COMPILER) -pa ../../ebin $(EFLAGS) -o ebin $<

docs:
	make edoc

clean:
	rm -f ebin/*.beam 



