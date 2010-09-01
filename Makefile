.SUFFIXES: .erl .beam .yrl
.PHONY: all main subdirs clean run-shell

# find all .erl files in ./src/, and compile them to the same structure in ./ebin/
ERL_SRC := $(shell find src -name *.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
SRC_SUBDIRS := $(shell find src -type d)
OBJ_SUBDIRS := $(patsubst src%,ebin%,${SRC_SUBDIRS})


all: subdirs main ebin/ircbot.app
main: ${ERL_OBJ}
subdirs: ${OBJ_SUBDIRS}

${OBJ_SUBDIRS}:
	mkdir $@

ebin/%.app: src/%.app
	cp $< $@

ebin/%.beam: src/%.erl
	erlc -o $(dir $@) $<


clean:
	rm -rf ebin/
	rm -f erl_crash.dump



run-shell:
	@erl -sname ircbot -pa ./ebin
