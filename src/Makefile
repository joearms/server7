.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

DEPS="tmp"

## leave for now

%.beam: %.erl
	erlc  -W $<

all: ehe_parser.beam beams

ehe_parser.erl: ehe_parser.yrl
	erlc ehe_parser.yrl

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf  *~ erl_crash.dump config
