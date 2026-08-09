-module(ircbot_api).
-author("gdamjan@gmail.com").

-export([new/1, pid/1, connect/1, disconnect/1, reconnect/1]).
-export([send_event/2, send_data/2, send_message/4]).
-export([privmsg/3, notice/3, join/2, part/2, ping/2, pong/2, nick/2]).
-export([add_plugin/3, delete_plugin/3, which_plugins/1]).

-opaque ref() :: {?MODULE, pid()}.
-export_type([ref/0]).

-spec new(pid()) -> ref().
new(IrcbotRef) ->
    {?MODULE, IrcbotRef}.

-spec pid(ref()) -> pid().
pid({?MODULE, IrcbotRef}) ->
    IrcbotRef.

-spec connect(ref()) -> ok.
connect({?MODULE, IrcbotRef}) ->
    gen_statem:cast(IrcbotRef, connect).

-spec disconnect(ref()) -> ok.
disconnect({?MODULE, IrcbotRef}) ->
    gen_statem:cast(IrcbotRef, disconnect).

-spec reconnect(ref()) -> ok.
reconnect({?MODULE, IrcbotRef}) ->
    disconnect({?MODULE, IrcbotRef}),
    connect({?MODULE, IrcbotRef}).


-spec add_plugin(module(), term(), ref()) -> ok.
add_plugin(Plugin, Args, {?MODULE, IrcbotRef}) ->
    gen_statem:call(IrcbotRef, {add_plugin, Plugin, Args}).

-spec delete_plugin(module(), term(), ref()) -> ok.
delete_plugin(Plugin, Args, {?MODULE, IrcbotRef}) ->
    gen_statem:call(IrcbotRef, {delete_plugin, Plugin, Args}).

-spec which_plugins(ref()) -> [module()].
which_plugins({?MODULE, IrcbotRef}) ->
    gen_statem:call(IrcbotRef, which_plugins).


-spec send_event(term(), ref()) -> ok.
send_event(Event, {?MODULE, IrcbotRef}) ->
    gen_statem:cast(IrcbotRef, Event).

-spec send_data(iodata(), ref()) -> ok.
send_data(Data, {?MODULE, IrcbotRef}) ->
    send_event({send, Data}, {?MODULE, IrcbotRef}).

-spec send_message(iodata(), iodata(), iodata(), ref()) -> ok.
send_message(Cmd, Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_data([Cmd, " ", Destination, " :", Msg], {?MODULE, IrcbotRef}).


-spec privmsg(iodata(), iodata(), ref()) -> ok.
privmsg(Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_message("PRIVMSG", Destination, Msg, {?MODULE, IrcbotRef}).

-spec notice(iodata(), iodata(), ref()) -> ok.
notice(Destination, Msg, {?MODULE, IrcbotRef}) ->
    send_message("NOTICE", Destination, Msg, {?MODULE, IrcbotRef}).

-spec join(iodata(), ref()) -> ok.
join(Channel, {?MODULE, IrcbotRef}) ->
    send_data(["JOIN ", Channel], {?MODULE, IrcbotRef}).

-spec part(iodata(), ref()) -> ok.
part(Channel, {?MODULE, IrcbotRef}) ->
    send_data(["PART ", Channel], {?MODULE, IrcbotRef}).

-spec ping(iodata(), ref()) -> ok.
ping(Server, {?MODULE, IrcbotRef}) ->
    send_data(["PING :", Server], {?MODULE, IrcbotRef}).

-spec pong(iodata(), ref()) -> ok.
pong(Server, {?MODULE, IrcbotRef}) ->
    send_data(["PONG :", Server], {?MODULE, IrcbotRef}).

-spec nick(iodata(), ref()) -> ok.
nick(Nick, {?MODULE, IrcbotRef}) ->
    send_data(["NICK ", Nick], {?MODULE, IrcbotRef}).
