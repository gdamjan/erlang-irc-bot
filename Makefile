ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

PLUGINS_SRC := $(wildcard src/plugins/*.erl)
PLUGINS_OBJ := $(patsubst src/plugins/%.erl,ebin/plugins/%.beam,${PLUGINS_SRC})


all: main plugins

main: ${ERL_OBJ}

plugins: ${PLUGINS_OBJ}

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -o ebin $<

ebin/plugins/%.beam: src/plugins/%.erl
	@mkdir -p ebin/plugins
	erlc -o ebin/plugins $<


clean:
	rm -f ebin/*.beam ebin/plugin/*.beam
	rm -f erl_crash.dump



run-shell:
	@erl -sname ircbot -pa ./ebin
