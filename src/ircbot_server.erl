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

start_new_connection(Config) ->
    {Host, Port} = Config#config.server,
    ircbot_connection:start_link(Host, Port).

handle_call(connect, _From, {Self, State, Config}) ->
    Pid = start_new_connection(Config),
    {reply, ok, {Self, State#state{conn=Pid}, Config}};

handle_call(disconnect, _From, S = {_Self, State, _Config}) ->
    Conn = State#state.conn,
    Conn ! {send_data, ["QUIT :", ?QUITMSG]},
    Conn ! quit,
    {reply, ok, S};


handle_call({add_plugin, Plugin, Args}, _From, S = {_Self, State, _Config}) ->
    case gen_event:add_handler(State#state.plugin_mgr, Plugin, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_msg("Problem loading plugin ~p ~p ~n", [Plugin, Reason]);
        Other ->
            error_logger:error_msg("Loading ~p reports ~p ~n", [Plugin, Other])
    end,
    {reply, ok, S};

handle_call({delete_plugin, Plugin, Args}, _From, S = {_Self, State, _Config}) ->
    gen_event:delete_handler(State#state.plugin_mgr, Plugin, Args),
    {reply, ok, S};

handle_call(which_plugins, _From, S = {_Self, State, _Config}) ->
    R = gen_event:which_handlers(State#state.plugin_mgr),
    {reply, R, S}.


handle_cast({connect_success, Conn}, {Self, State, Config}) ->
    Conn ! {send_data, ["NICK ", Config#config.nickname]},
    Conn ! {send_data, ["USER ", Config#config.nickname, " 8 * :", ?REALNAME]},
    gen_event:notify(State#state.plugin_mgr, {Self, online}),
    {noreply, {Self, State#state{conn=Conn}, Config}};

handle_cast({send_data, Data}, S = {_Self, State, _Config}) ->
    Conn = State#state.conn,
    Conn ! {send_data, Data},
    {noreply, S};

handle_cast({received_data, Data}, S = {Self, State, _Config}) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Data),
    gen_event:notify(State#state.plugin_mgr, {in, Self, IrcMessage}), % notify all plugins
    {noreply, S}.

%% handle the EXIT of the connection process
handle_info({'EXIT', Pid, normal}, {Self, State=#state{conn=Pid}, Config}) ->
    gen_event:notify(State#state.plugin_mgr, {Self, offline}),
    NewPid = start_new_connection(Config),
    {noreply, {Self, State#state{conn=NewPid}, Config}};


%% handle unknown messages
handle_info(Msg, State) ->
    io:format("UNK: ~w~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, {Self, State, Config}, _Extra) ->
    if
        is_pid(State#state.conn) ->
            State#state.conn ! code_switch;
        true ->
            ok
    end,
    {ok, {Self, State, Config}}.

terminate(_Reason, _State) -> ok.
