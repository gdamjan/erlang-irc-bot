Quick start to using the ircbot
===============================

To compile everything run:

    make

Edit and rename the settings.cfg.sample file (to settings.cfg), then start 
an Erlang REPL shell, with the path set to the .beam files:

    make run-shell

or

    erl -pa ./ebin

Once in the Erlang REPL you can start the bot with:
    
    {ok, IrcBot} = ircbot_server:start_link("settings.cfg").
    gen_server:call(IrcBot, connect).
    gen_server:call(IrcBot, {add_plugin, 'plugins.rps', []}).

You can make changes to the plugins while the bot is running. Just hit "make" in 
another terminal and then, if everything is ok, in the erlang REPL run:

    l('plugins.rps').

Erlangs code switching and gen_event framework will handle all the details to
run the new code without even disconnecting.
