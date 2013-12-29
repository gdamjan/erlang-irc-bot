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

    rebar get-deps
    rebar compile

Second, edit and rename the settings.cfg.sample file (to settings.cfg). Then start
an Erlang REPL shell. Make sure the module path is set to the ./ebin/
directory, where all the compiled .beam files are:

    ERL_LIBS=.:./deps erl


Once in the Erlang REPL you can start the bot with:

    {ok, Settings} = file:consult("settings.cfg").
    {ok, IrcBot} = ircbot_fsm:start(Settings).
    gen_fsm:send_event(IrcBot, connect).
    gen_fsm:sync_send_all_state_event(IrcBot, {add_plugin, 'ircbot_plugin_rps', []}).

You can make changes to the source code & plugins while the bot is running.
Just hit "rebar compile" in another terminal and then, if everything is ok, in the Erlang REPL run:

    l('ircbot_plugin_rps').

to reload the 'ircbot_plugin_rps' rock-paper-scissors module.

or

    l(ircbot_fsm).

to reload the 'ircbot_fsm' module.


Erlangs [code switching][code switching] and the gen_fsm/gen_event frameworks
will handle all the details to run the new code without even disconnecting.

[code switching]: http://en.wikipedia.org/wiki/Erlang_%28programming_language%29#Hot_code_loading_and_modules


Parametrized Module API
-----------------------

Using the parametrized module support in Erlang we can do something like this
too:

    {ok, Settings} = file:consult("settings.cfg").
    IrcBot = ircbot_fsm:new(Settings).
    IrcBot:connect().
    IrcBot:add_plugin(ircbot_plugin_rps, []).


The parametrized module feature is officially supported since Erlang R14, so
now I consider this the official API of the bot.


Real OTP Application
--------------------

To start it:

    erl -sname ircbot@localhost -setcookie xxx -pa ebin/ \
        -sasl errlog_type error -s ircbot_app -conf settings-app.cfg

To connect a remote shell to it:

    erl -sname ctrl -remsh ircbot@localhost -setcookie xxx


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
