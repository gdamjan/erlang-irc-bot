.SUFFIXES: .erl .beam .yrl
.PHONY: all main subdirs clean run-shell

# find all .erl files in ./src/, and compile them to the same structure in ./ebin/
ERL_SRC := $(shell find src -name '*.erl')
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
SRC_SUBDIRS := $(shell find src -type d)
OBJ_SUBDIRS := $(patsubst src%,ebin%,${SRC_SUBDIRS})

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .dialyzer_plt

all: compile ebin/ircbot.app
compile: ${OBJ_SUBDIRS} ${ERL_OBJ}

${OBJ_SUBDIRS}:
	mkdir $@

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	erlc ${ERLC_FLAGS} -o $(dir $@) $<

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) ebin

dialyzer: compile
	@echo Compile with "'make ERLC_FLAGS=+debug_info'" prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

clean:
	rm -rf ebin/
	rm -f erl_crash.dump



run-shell:
	@erl -sname ircbot -pa ./ebin
