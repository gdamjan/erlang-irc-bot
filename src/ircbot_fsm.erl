-module(ircbot_fsm).
-author("gdamjan@gmail.com").

-behaviour(gen_fsm).
-include("ircbot.hrl").

-record(config, {nickname, server}).
-record(slaves, {conn, plugins}).

%%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
-export([offline/2, offline/3, connecting/2, registering/2,
        online/2, online/3, disconnecting/2, disconnecting/3]).

%%% public api
-export([new/1, start/1, start_link/1]).

% api to use in the shell
new(Settings) ->
    {ok, Ref} = start(Settings),
    ircbot_api:new(Ref).

start(Settings) ->
    gen_fsm:start(?MODULE, Settings, []).

%% api for use in supervisors
start_link(Settings) ->
    gen_fsm:start_link(?MODULE, Settings, []).

%% internal functions
get_config(Settings) ->
    Nick = proplists:get_value(nickname, Settings),
    {Host, Port} = proplists:get_value(server, Settings),
    #config{nickname=Nick, server={Host, Port}}.

start_connection({Host, Port}) ->
    Self = ircbot_api:new(self()),
    ircbot_connection:start_link(Self, Host, Port).

send(Conn, Data) ->
    Conn ! {send, Data}.

quit(Conn) ->
    send(Conn, ["QUIT :", ?QUITMSG]),
    Conn ! quit.

registerize(Conn, Nickname) ->
    send(Conn, ["NICK ", Nickname]),
    send(Conn, ["USER ", Nickname, " 8 * :", ?REALNAME]).


%% gen_fsm callbacks
init(Settings) ->
    process_flag(trap_exit, true),
    {ok, Plugins} = ircbot_plugins:start_link(Settings),
    Config = get_config(Settings),
    Slaves = #slaves{plugins=Plugins},
    {ok, offline, {Slaves, Config}}.


offline(connect, From, {Slaves, Config}) ->
    Pid = start_connection(Config#config.server),
    {next_state, connecting, [From, {Slaves#slaves{conn=Pid}, Config}]};

offline(_, _, S) ->
    {reply, ok, offline, S}.

offline(_, S) ->
    {next_state, offline, S}.


connecting({connected, Pid}, [From, {Slaves, Config}]) ->
    gen_fsm:reply(From, ok),
    connecting({connected, Pid}, {Slaves, Config});

connecting({connected, Pid}, {Slaves, Config}) ->
    registerize(Pid, Config#config.nickname),
    {next_state, registering, {Slaves#slaves{conn=Pid}, Config}, 30000};

connecting(Event, S) ->
    io:format("FIXME[connecting]: ~p~n", [Event]),
    {next_state, offline, S}.


registering({received, Line}, {Slaves, Config}) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Line),
    case IrcMessage of
        [_, _, <<"001">>, _, _] ->
            Self = ircbot_api:new(self()),
            ircbot_plugins:notify(Slaves#slaves.plugins, {Self, online}),
            ircbot_plugins:notify(Slaves#slaves.plugins, {in, Self, IrcMessage}),
            {next_state, online, {Slaves, Config}};
        _ ->
            {next_state, registering, {Slaves, Config}, 30000}
    end;

registering(Event, S) ->
    io:format("FIXME[registering]: ~p~n", [Event]),
    {next_state, offline, S}.


online({send_data, Data}, {Slaves, Config}) ->
    Conn = Slaves#slaves.conn,
    send(Conn, Data),
    {next_state, online, {Slaves, Config}};

online({received, Line}, {Slaves, Config}) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Line),
    Self = ircbot_api:new(self()),
    ircbot_plugins:notify(Slaves#slaves.plugins, {in, Self, IrcMessage}), % notify all plugins
    {next_state, online, {Slaves, Config}}.

online(disconnect, From, {Slaves, Config}) ->
    Self = ircbot_api:new(self()),
    ircbot_plugins:notify(Slaves#slaves.plugins, {Self, offline}),
    Conn = Slaves#slaves.conn,
    quit(Conn),
    {next_state, disconnecting, [From, {Slaves, Config}], 50000}.

disconnecting(timeout, [From, {Slaves, Config}]) ->
    gen_fsm:reply(From, timeout),
    {next_state, offline, {Slaves, Config}};

disconnecting(_, S) ->
    {next_state, disconnecting, S}.

disconnecting(_, _, S) ->
    {reply, ok, disconnecting, S}.


%% handle the EXIT of the connection process
handle_info({'EXIT', Pid, normal}, offline, {Slaves=#slaves{conn=Pid}, Config}) ->
    {next_state, offline, {Slaves, Config}};

handle_info({'EXIT', Pid, normal}, disconnecting, [From, {Slaves=#slaves{conn=Pid}, Config}]) ->
    io:format("Exit in disconnecting: ~p~n", [Pid]),
    gen_fsm:reply(From, ok),
    {next_state, offline, {Slaves, Config}};

handle_info({'EXIT', Pid, normal}, online, {Slaves=#slaves{conn=Pid}, Config}) ->
    io:format("Exit in online: ~p~n", [Pid]),
    Self = ircbot_api:new(self()),
    ircbot_plugins:notify(Slaves#slaves.plugins, {Self, offline}),
    NewPid = start_connection(Config#config.server),
    {next_state, connecting, {Slaves#slaves{conn=NewPid}, Config}};

handle_info({'EXIT', Pid, normal}, Name, {Slaves=#slaves{conn=Pid}, Config}) ->
    io:format("Exit in ~p: ~p~n", [Name, Pid]),
    NewPid = start_connection(Config#config.server),
    {next_state, connecting, {Slaves#slaves{conn=NewPid}, Config}};


%% handle unknown messages
handle_info(Msg, StateName, StateData) ->
    io:format("UNK: ~p | ~p | ~w~n", [StateName, StateData, Msg]),
    {next_state, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% Plugin managemenet
handle_sync_event({add_plugin, Plugin, Args}, _From, StateName, StateData) ->
    {Slaves, _Config} = StateData,
    ircbot_plugins:add_handler(Slaves#slaves.plugins, Plugin, Args),
    {reply, ok, StateName, StateData};

handle_sync_event({delete_plugin, Plugin, Args}, _From, StateName, StateData) ->
    {Slaves, _Config} = StateData,
    ircbot_plugins:delete_handler(Slaves#slaves.plugins, Plugin, Args),
    {reply, ok, StateName, StateData};

handle_sync_event(which_plugins, _From, StateName, StateData) ->
    {Slaves, _Config} = StateData,
    Reply = ircbot_plugins:which_handlers(Slaves#slaves.plugins),
    {reply, Reply, StateName, StateData}.


%% OTP handlers
code_change(_OldVsn, offline, {Slaves, Config}, _Extra) ->
    {ok, offline, {Slaves, Config}};

code_change(_OldVsn, StateName, {Slaves, Config}, _Extra) ->
    Slaves#slaves.conn ! code_switch,
    {ok, StateName, {Slaves, Config}}.


terminate(_Reason, _StateName, _StateData) -> ok.
