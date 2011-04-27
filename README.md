An extensible ircbot written in Erlang
======================================

It all started when I decided I need to learn Erlang. At the same time I needed
a simple ircbot to handle some of the channels I frequent on. These two things
put together and I decided to start this project.

The evolution of my knowledge can be best seen from the history of the [git
commits][commits].

[commits]: http://github.com/gdamjan/erlang-irc-bot/commits/master

Now that the bot is extensible by plug-ins, and quite stable it's becoming pretty
useful. It still needs improvements and is work in progress, but plug-ins can
be written for anything.


Patches, help and feature requests can be sent on the github issue tracker.
There's a TODO list I keep there too.


The bot is MIT licensed (for no particular reason), it's a very liberal license
with no strings, so you can really do whatever you want with it.


Quick start
-----------

First, compile everything:

    make

Second, edit and rename the settings.cfg.sample file (to settings.cfg). Then start
an Erlang REPL shell. Make sure the module path is set to the ./ebin/
directory, where all the compiled .beam files are:

    erl -pa ./ebin

or simply:

    make run-shell

Once in the Erlang REPL you can start the bot with:

    {ok, Settings} = file:consult("settings.cfg").
    {ok, IrcBot} = ircbot_fsm:start(Settings).
    gen_fsm:send_event(IrcBot, connect).
    gen_fsm:sync_send_all_state_event(IrcBot, {add_plugin, 'plugins.rps', []}).

You can make changes to the source code & plugins while the bot is running.
Just hit "make" in another terminal and then, if everything is ok, in the Erlang REPL run:

    l('plugins.rps').

or

    l(ircbot_fsm).

Erlangs [code switching][code switching] and the gen_fsm/gen_event frameworks
will handle all the details to run the new code without even disconnecting.

[code switching]: http://en.wikipedia.org/wiki/Erlang_%28programming_language%29#Hot_code_loading_and_modules

Experimental OOP API (*)
------------------------

Using the parametrized module support in Erlang we can do something like this
too:

    {ok, Settings} = file:consult("settings.cfg").
    IrcBot = ircbot_fsm:new(Settings).
    IrcBot:connect().
    IrcBot:add_plugin(plugins.rps, []).

I'm yet to decide if it's smart to do this, since this feature is considered
experimental in Erlang. (*) The parametrized module feature is becoming
officially supported in Erlang R14, so I'm deciding that it will be the official
API of this bot.


Similar projects
________________

* http://github.com/wrboyce/erb
* http://manderlbot.org/
* http://erlirc.com/
* http://github.com/jimm/erlang-ircbot
* https://bitbucket.org/john_b/erlang-ircbot
* http://github.com/pizza/abbot
* http://code.google.com/p/madcow/
* http://www.otfbot.org/
* https://github.com/mazenharake/eirc/