-module(ircbot_api).
-author("gdamjan@gmail.com").

-export([new/1, pid/1, connect/1, disconnect/1, reconnect/1]).
-export([send_event/2, send_data/2, send_message/4]).
-export([privmsg/3, notice/3, join/2, part/2, ping/2, pong/2]).
-export([add_plugin/3, delete_plugin/3, which_plugins/1]).

new(IrcbotRef) ->
    {?MODULE, IrcbotRef}.

pid({?MODULE, IrcbotRef}) ->
    IrcbotRef.

connect({?MODULE, IrcbotRef}) ->
    gen_fsm:send_event(IrcbotRef, connect).

disconnect({?MODULE, IrcbotRef}) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, disconnect).

reconnect({?MODULE, IrcbotRef}) ->
    disconnect({?MODULE, IrcbotRef}),
    connect({?MODULE, IrcbotRef}).


add_plugin(Plugin, Args, {?MODULE, IrcbotRef}) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, {add_plugin, Plugin, Args}).

delete_plugin(Plugin, Args, {?MODULE, IrcbotRef}) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, {delete_plugin, Plugin, Args}).

which_plugins({?MODULE, IrcbotRef}) ->
    gen_fsm:sync_send_all_state_event(IrcbotRef, which_plugins).


send_event(Event, {?MODULE, IrcbotRef}) ->
    gen_fsm:send_event(IrcbotRef, Event).

send_data(Data, {?MODULE, IrcbotRef}) ->
    send_event({send, Data}, {?MODULE, IrcbotRef}).

send_message(Cmd, Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_data([Cmd, " ", Destination, " :", Msg], {?MODULE, IrcbotRef}).


privmsg(Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_message("PRIVMSG", Destination, Msg, {?MODULE, IrcbotRef}).

notice(Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_message("NOTICE", Destination, Msg, {?MODULE, IrcbotRef}).

join(Channel, {?MODULE, IrcbotRef}) ->
    send_data(["JOIN ", Channel], {?MODULE, IrcbotRef}).

part(Channel, {?MODULE, IrcbotRef}) ->
    send_data(["PART ", Channel], {?MODULE, IrcbotRef}).

ping(Server, {?MODULE, IrcbotRef}) ->
    send_data(["PING :", Server], {?MODULE, IrcbotRef}).

pong(Server, {?MODULE, IrcbotRef}) ->
    send_data(["PONG :", Server], {?MODULE, IrcbotRef}).
