all: compile

compile:
	mkdir -p ebin/plugins
	for i in src/*.erl; do erlc -o ./ebin $$i; done
	for i in src/plugins/*.erl; do erlc -o ./ebin/plugins $$i; done

clean:
	rm -f ebin/*.beam ebin/plugin/*.beam
	rm -f erl_crash.dump

run:
	erl -sname ircbot -pa ./ebin
