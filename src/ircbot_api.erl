-module(ircbot_api).
-author("gdamjan@gmail.com").

-export([connect/1, disconnect/1, reconnect/1]).
-export([send_event/2, send_data/2, send_message/4]).
-export([privmsg/3, notice/3, join/2, part/2, ping/2, pong/2, nick/2]).
-export([add_plugin/3, delete_plugin/3, which_plugins/1]).

connect(IrcbotRef) ->
    gen_statem:cast(IrcbotRef, connect).

disconnect(IrcbotRef) ->
    gen_statem:cast(IrcbotRef, disconnect).

reconnect(IrcbotRef) ->
    disconnect(IrcbotRef),
    connect(IrcbotRef).


add_plugin(Plugin, Args, IrcbotRef) ->
    gen_statem:call(IrcbotRef, {add_plugin, Plugin, Args}).

delete_plugin(Plugin, Args, IrcbotRef) ->
    gen_statem:call(IrcbotRef, {delete_plugin, Plugin, Args}).

which_plugins(IrcbotRef) ->
    gen_statem:call(IrcbotRef, which_plugins).


send_event(Event, IrcbotRef) ->
    gen_statem:cast(IrcbotRef, Event).

send_data(Data, IrcbotRef) ->
    send_event({send, Data}, IrcbotRef).

send_message(Cmd, Destination, Msg, IrcbotRef) ->
    send_data([Cmd, " ", Destination, " :", Msg], IrcbotRef).


privmsg(Destination, Msg, IrcbotRef) ->
    send_message("PRIVMSG", Destination, Msg, IrcbotRef).

notice(Destination, Msg, IrcbotRef) ->
    send_message("NOTICE", Destination, Msg, IrcbotRef).

join(Channel, IrcbotRef) ->
    send_data(["JOIN ", Channel], IrcbotRef).

part(Channel, IrcbotRef) ->
    send_data(["PART ", Channel], IrcbotRef).

ping(Server, IrcbotRef) ->
    send_data(["PING :", Server], IrcbotRef).

pong(Server, IrcbotRef) ->
    send_data(["PONG :", Server], IrcbotRef).

nick(Nick, IrcbotRef) ->
    send_data(["NICK ", Nick], IrcbotRef).
