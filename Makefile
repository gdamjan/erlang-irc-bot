.SUFFIXES: .erl .beam .yrl

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

PLUGINS_SRC := $(wildcard src/plugins/*.erl)
PLUGINS_OBJ := $(patsubst src/plugins/%.erl,ebin/plugins/%.beam,${PLUGINS_SRC})


all: main plugins

ebin:
	@mkdir -p ebin
ebin/plugins:
	@mkdir -p ebin/plugins

main: ebin ${ERL_OBJ}
plugins: ebin/plugins ${PLUGINS_OBJ}


ebin/%.beam: src/%.erl
	erlc -o `dirname $@` $<


clean:
	rm -rf ebin/
	rm -f erl_crash.dump



run-shell:
	@erl -sname ircbot -pa ./ebin
