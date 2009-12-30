-module(ircbot_server).
-behaviour(gen_server).
-author('gdamjan@gmail.com').

-include_lib("ircbot.hrl").
-record(config, {nickname, server, channels}).
-record(state, {sock, plugin_mgr}).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).



%% gen_server callbacks
init(Settings) ->
    Config = get_config(Settings),
    {ok, Plugins} = init_plugins(Settings),
    State = #state{sock=none, plugin_mgr=Plugins},
    {ok, {State, Config}}.

get_config(Settings) ->
    Nick = proplists:get_value(nickname, Settings),
    {Host, Port} = proplists:get_value(server, Settings),
    Chans = proplists:get_value(channels, Settings, []),
    #config{nickname=Nick, server={Host, Port}, channels=Chans}.

init_plugins(Settings) ->
    {ok, Plugins} = gen_event:start_link(),
    gen_event:add_handler(Plugins, pong_plugin, []),
    gen_event:add_handler(Plugins, ctcp_plugin, []),
    lists:foreach(
        fun ({Plugin, Args}) ->
            gen_event:add_handler(Plugins, Plugin, Args)
        end,
        proplists:get_value(plugins, Settings, [])
    ),
    {ok, Plugins}.


handle_call(connect, _From, {State, Config}) ->
    {Host, Port} = Config#config.server,
    {ok, Sock} = gen_tcp:connect(Host, Port,
            [binary, {active, true}, {packet, line}], ?TCPTIMEOUT),
    send_msg(Sock, ["NICK ", Config#config.nickname]),
    send_msg(Sock, ["USER ", Config#config.nickname, " 8 * :", ?REALNAME]),
    lists:foreach(
        fun (Ch) ->
            send_msg(Sock, ["JOIN ", Ch])
        end,
        Config#config.channels
    ),
    {reply, ok, {State#state{sock=Sock}, Config}};

handle_call(disconnect, _From, {State, Config}) ->
    Sock = State#state.sock,
    send_msg(Sock, ["QUIT :", ?QUITMSG]),
    gen_tcp:close(Sock),
    {reply, ok, {State#state{sock=none}, Config}};

handle_call({add_plugin, Plugin, Args},  _From, {State, Config}) ->
    gen_event:add_handler(State#state.plugin_mgr, Plugin, Args),
    {reply, ok, {State, Config}}.

handle_cast({send_data, Data}, {State, Config}) ->
    send_msg(State#state.sock, Data),
    {noreply, {State, Config}}.


%% handle socket data
handle_info({tcp, _Sock, Data}, {State, Config}) ->
    [Line|_Tail] = re:split(Data, "\r\n"), % strip the CRNL at the end
    debug(in, [Line]),    % for debuging only
    IrcMessage = utils:irc_parse(Line),
    gen_event:notify(State#state.plugin_mgr, {self(), IrcMessage}), % notify all plugins
    {noreply, {State, Config}};


%% handle errors on the socket
handle_info({tcp_error, Sock, Reason}, State) ->
    io:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]),
    {noreply, State};

handle_info({tcp_closed, Sock}, State) ->
    io:format("Socket ~w closed [~w]~n", [Sock, self()]),
    {noreply, State};


%% unknown messages
handle_info(Msg, State) ->
    io:format("UNK: ~w~n", [Msg]),
    {noreply, State}.



terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% helpers
send_msg(Sock, Message) ->
    debug(out, [Message]), % for debuging only
    gen_tcp:send(Sock, [Message, ?CRNL]).

%% debug helpers
debug(in, Msg) ->
    utils:debug([" IN| ", Msg]);

debug(out, Msg) ->
    utils:debug(["OUT| ", Msg]).
