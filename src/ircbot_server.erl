-module(ircbot_server).
-behaviour(gen_server).
-author('gdamjan@gmail.com').

-include_lib("ircbot.hrl").
-record(config, {nickname, server}).
-record(state, {conn, plugin_mgr}).


%% API
-export([new/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

new(Settings) ->
    {ok, Ref} = start_link(Settings),
    ircbot_api:new(Ref).

start_link(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).


get_config(Settings) ->
    Nick = proplists:get_value(nickname, Settings),
    {Host, Port} = proplists:get_value(server, Settings),
    #config{nickname=Nick, server={Host, Port}}.

%% Start the plugin manager (gen_event)
init_plugins(Settings) ->
    {ok, Plugins} = gen_event:start_link(),
    Channels = proplists:get_value(channels, Settings, []),
    gen_event:add_handler(Plugins, pong_plugin, []),
    gen_event:add_handler(Plugins, ctcp_plugin, []),
    gen_event:add_handler(Plugins, channels_plugin, Channels),
    lists:foreach(
        fun ({Plugin, Args}) ->
            gen_event:add_handler(Plugins, Plugin, Args)
        end,
        proplists:get_value(plugins, Settings, [])
    ),
    {ok, Plugins}.


%% gen_server callbacks
init(Settings) ->
    process_flag(trap_exit, true),
    Config = get_config(Settings),
    {ok, Plugins} = init_plugins(Settings),
    State = #state{conn=none, plugin_mgr=Plugins},
    Self = ircbot_api:new(self()),
    {ok, {Self, State, Config}}.


handle_call(connect, _From, {Self, State, Config}) ->
    {Host, Port} = Config#config.server,
    Connection = ircbot_connection:start_link(self(), Host, Port),
    Connection ! {send_data, ["NICK ", Config#config.nickname]},
    Connection ! {send_data, ["USER ", Config#config.nickname, " 8 * :", ?REALNAME]},
    {reply, ok, {Self, State#state{conn=Connection}, Config}};

handle_call(disconnect, _From, {Self, State, Config}) ->
    Connection = State#state.conn,
    Connection ! {send_data, ["QUIT :", ?QUITMSG]},
    Connection ! quit,
    {reply, ok, {Self, State#state{conn=none}, Config}};


handle_call({add_plugin, Plugin, Args}, _From, {Self, State, Config}) ->
    gen_event:add_handler(State#state.plugin_mgr, Plugin, Args),
    {reply, ok, {Self, State, Config}};

handle_call({delete_plugin, Plugin, Args}, _From, {Self, State, Config}) ->
    gen_event:delete_handler(State#state.plugin_mgr, Plugin, Args),
    {reply, ok, {Self, State, Config}};

handle_call(which_plugins, _From, {Self, State, Config}) ->
    R = gen_event:which_handlers(State#state.plugin_mgr),
    {reply, R, {Self, State, Config}}.


handle_cast({send_data, Data}, {Self, State, Config}) ->
    Connection = State#state.conn,
    Connection ! {send_data, Data},
    {noreply, {Self, State, Config}};

handle_cast({received_data, Data}, {Self, State, Config}) ->
    IrcMessage = utils:irc_parse(Data),
    gen_event:notify(State#state.plugin_mgr, {Self, IrcMessage}), % notify all plugins
    {noreply, {Self, State, Config}}.


%% handle the EXIT of the connection process
handle_info({'EXIT', Pid, normal}, {Self, State=#state{conn=Pid}, Config}) ->
    {reply, ok, NewState} =  handle_call(connect, none, {Self, State#state{conn=none}, Config}),
    {noreply, NewState};


%% handle unknown messages
handle_info(Msg, State) ->
    io:format("UNK: ~w~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
