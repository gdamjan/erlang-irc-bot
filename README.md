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
```
rebar3 compile
```

Second, edit and rename the `settings.cfg.example` file to `settings.cfg`. Then start
an Erlang REPL shell:
```
rebar3 shell
```
Once in the Erlang REPL you can start the bot with:
```
    {ok, Settings} = file:consult("settings.cfg").
    [Args|_] = lists:filtermap(
        fun(El) ->
            case El of
                {connection, Args} -> {true, Args};
                _ -> false
            end
    end, Settings).
    {ok, IrcBot} = ircbot_statem:start_link(Args).
    gen_statem:cast(IrcBot, connect).
    gen_statem:call(IrcBot, {add_plugin, ircbot_plugin_doesnt, []}).
```

You can make changes to the source code & plugins while the bot is running.
Just hit `rebar3 compile` in another terminal and then, if everything is ok, in the Erlang REPL run:

    l(ircbot_plugin_help).

to reload the `ircbot_plugin_help` module.

or

    l(ircbot_statem).

to reload the `ircbot_statem` module.


Erlangs [code switching][code switching] and the gen_statem/gen_event frameworks
will handle all the details to run the new code without even disconnecting.

[code switching]: http://en.wikipedia.org/wiki/Erlang_%28programming_language%29#Hot_code_loading_and_modules


# Real OTP Application
```
rebar3 release
./_build/default/rel/ircbot/bin/ircbot -conf $PWD/settings.cfg
```
