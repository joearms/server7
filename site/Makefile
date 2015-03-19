.SUFFIXES: .erl .beam .web

WEBS := $(wildcard *.web)
MODS := $(wildcard *.erl)

DEPS="tmp"

## leave for now

%.beam: %.erl
	erlc  -W $<

../tmp/%.erl: %.web
	erl -pa ../src -s app_compiler_vsn2 batch $<
	erlc -o ../tmp $@

all: webs beams

beams: ${MODS:%.erl=%.beam}

webs: ${WEBS:%.web=../tmp/%.erl}

clean:
	rm -rf  *~ erl_crash.dump config
