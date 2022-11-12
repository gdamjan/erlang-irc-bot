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
rebar3 shell --start-clean
```

Once in the Erlang REPL you can start the bot with:

```
{ok, [{ connection, Settings }]} = file:consult("settings.cfg").
{ok, IrcBot} = ircbot_fsm:start(Settings).
gen_fsm:sync_send_all_state_event(IrcBot, {add_plugin, ircbot_plugin_uptime, []}).
```

You can make changes to the source code & plugins while the bot is running.
Just hit "rebar3 compile" in another terminal and then, if everything is ok, in the Erlang REPL run:
```
l(ircbot_plugin_uptime).
```
to reload the `ircbot_plugin_uptime` uptime module.

or
```
l(ircbot_fsm).
```
to reload the `ircbot_fsm` module.


Erlangs [code switching][code switching] and the gen_fsm/gen_event frameworks
will handle all the details to run the new code without even disconnecting.

[code switching]: http://en.wikipedia.org/wiki/Erlang_%28programming_language%29#Hot_code_loading_and_modules


Real OTP Application
--------------------

```
rebar3 as prod release
```

An Erlang release will be in `_build/prod/rel/ircbot/`, an alternative is to get a tarball with `rebar3 as prod tar`.
