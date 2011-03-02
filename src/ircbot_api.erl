%% this is an Erlang parameterized module
%% it kind of mimics Object Oriented syntax

-module(ircbot_api, [IrcbotRef]).
-author("gdamjan@gmail.com").

-export([pid/0, connect/0, connect/1, disconnect/0, reconnect/0]).
-export([send_event/1, send_data/1, send_message/3]).
-export([privmsg/2, notice/2, join/1, part/1, ping/1, pong/1]).
-export([add_plugin/2, delete_plugin/2, which_plugins/0]).


pid() ->
    IrcbotRef.

connect() ->
    connect(infinity).

connect(Timeout) ->
    gen_fsm:sync_send_event(IrcbotRef, connect, Timeout).

disconnect() ->
    gen_fsm:sync_send_event(IrcbotRef, disconnect).

reconnect() ->
    disconnect(),
    connect().


add_plugin(Plugin, Args) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, {add_plugin, Plugin, Args}).

delete_plugin(Plugin, Args) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, {delete_plugin, Plugin, Args}).

which_plugins() ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, which_plugins).


send_event(Event) ->
    gen_fsm:send_event(IrcbotRef, Event).

send_data(Data) ->
    send_event({send_data, Data}).

send_message(Cmd, Destination, Msg) ->
    send_data([Cmd, " ", Destination, " :", Msg]).


privmsg(Destination, Msg) ->
    send_message("PRIVMSG", Destination, Msg).

notice(Destination, Msg) ->
    send_message("NOTICE", Destination, Msg).

join(Channel) ->
    send_data(["JOIN ", Channel]).

part(Channel) ->
    send_data(["PART ", Channel]).

ping(Server) ->
    send_data(["PING :", Server]).

pong(Server) ->
    send_data(["PONG :", Server]).
