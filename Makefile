.SUFFIXES: .erl .beam .yrl

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(ERL_SRC:src/%.erl=ebin/%.beam)

PLUGINS_SRC := $(wildcard src/plugins/*.erl)
PLUGINS_OBJ := $(PLUGINS_SRC:src/plugins/%.erl=ebin/plugins/%.beam)


all: main plugins
main: ebin/ ${ERL_OBJ}
plugins: ebin/plugins/ ${PLUGINS_OBJ}


ebin/:
	@mkdir -p ebin
ebin/plugins/:
	@mkdir -p ebin/plugins


ebin/%.beam: src/%.erl
	erlc -o `dirname $@` $<


clean:
	rm -rf ebin/
	rm -f erl_crash.dump



run-shell:
	@erl -sname ircbot -pa ./ebin
