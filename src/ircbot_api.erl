%% this is an Erlang parameterized module
%% it kind of mimics Object Oriented syntax

-module(ircbot_api, [IrcbotRef]).
-author('gdamjan@gmail.com').

-export([connect/0, disconnect/0, reconnect/0, add_plugin/2, which_plugins/0, send_data/1]).


connect() ->
    gen_server:call(IrcbotRef, connect).

disconnect() ->
    gen_server:call(IrcbotRef, disconnect).

reconnect() ->
    disconnect(),
    connect().

add_plugin(Plugin, Args) ->
    gen_server:call(IrcbotRef, {add_plugin, Plugin, Args}).

which_plugins() ->
    gen_server:call(IrcbotRef, which_plugins).

send_data(Data) ->
    gen_server:cast(IrcbotRef, {send_data, Data}).

% save_state(Filename) ->
%     State = gen_event:call(IrcbotRef, get_state),
%     file:write(Filename, State).
