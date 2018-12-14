COMPILER=erlc

vpath %.erl 

BEAMS = $(patsubst %.erl,ebin/%.beam,$(wildcard *.erl))

HEADERS = $(wildcard *.hrl)
EFLAGS = +debug_info 

help:
	@echo "make all # makes all"
	@echo "make run"

run: main
	erl -pa ebin -pa ../../ebin

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

.PHONY: all run main clean

