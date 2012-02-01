.SUFFIXES: .erl .beam .yrl
.PHONY: all clean run-shell


SRCDIR  := src
OBJDIR  := ebin

ERLC    := erlc $(ERLC_FLAGS)

# GNU Make Recursive Wildcard Function
rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))

SOURCES := $(call rwildcard,$(SRCDIR),*.erl)
OBJECTS := $(patsubst $(SRCDIR)/%.erl,$(OBJDIR)/%.beam,$(SOURCES))
OBJDIRS := $(sort $(foreach a,$(OBJECTS),$(dir $a)))


all: $(OBJDIRS) $(OBJECTS) $(OBJDIR)/ircbot.app

$(OBJDIRS):
	mkdir $@

$(OBJDIR)/%.app: $(SRCDIR)/%.app.src
	cp $< $@

$(OBJDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -o $(dir $@) $<

clean:
	rm -rf $(OBJDIR)
	rm -f erl_crash.dump

run-shell:
	@erl -sname ircbot -pa $(OBJDIR)


# dialyzer support
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
       xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) $(OBJDIR)

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) $(OBJDIR)

dialyzer: compile
	@echo Compile with "'make ERLC_FLAGS=+debug_info'" prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) $(OBJDIR)
