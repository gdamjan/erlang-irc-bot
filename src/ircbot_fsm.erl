-module(ircbot_fsm).
-author("gdamjan@gmail.com").

-behaviour(gen_fsm).

%%% public api
-export([new/1, start/1, new_link/1, start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
          terminate/3, code_change/4]).
%% states
-export([standby/2, ready/2, connecting/2, registering/2]).

-include("ircbot.hrl").
-record(state, {
        nickname,
        password,
        server,
        plugins,
        connection,
        backoff,
        timer,
        ssl
    }).


%% PM api to use in the shell
new(Settings) ->
    {ok, Ref} = start(Settings),
    ircbot_api:new(Ref).

new_link(Settings) ->
    {ok, Ref} = start_link(Settings),
    ircbot_api:new(Ref).

%% traditional OTP api
start(Settings) ->
    case proplists:get_value(name, Settings) of
        undefined ->
            gen_fsm:start(?MODULE, Settings, []);
        FsmName ->
            gen_fsm:start(FsmName, ?MODULE, Settings, [])
    end.

start_link(Settings) ->
    case proplists:get_value(name, Settings) of
        undefined ->
            gen_fsm:start_link(?MODULE, Settings, []);
        FsmName ->
            gen_fsm:start_link(FsmName, ?MODULE, Settings, [])
    end.


%%% gen_fsm init/1
%%% `Settings` should be a proplist ussually created from a
%%% config file with file:consult
init(Settings) ->
    case proplists:get_value(ssl, Settings) of
        true ->
            Ssl = true;
        _ ->
            Ssl = false
    end,
    Nick = proplists:get_value(nickname, Settings),
    Password = proplists:get_value(password, Settings),
    Server = proplists:get_value(server, Settings),
    {ok, Plugins} = ircbot_plugins:start_link(Settings),
    StateData = #state{nickname=Nick,password=Password,server=Server,plugins=Plugins,backoff=0,ssl=Ssl},
    gen_fsm:send_event(self(), connect),
    {ok, standby, StateData}.


%%% helpers
send(Conn, Data) ->
    Conn ! {send, Data}.

send_quit(Conn) ->
    send(Conn, ["QUIT :", ?QUITMSG]),
    Conn ! quit.

send_login(Conn, Nickname, undefined) ->
    send(Conn, ["NICK ", Nickname]),
    send(Conn, ["USER ", Nickname, " 8 * :", ?REALNAME]);

send_login(Conn, Nickname, Password) ->
    send(Conn, ["PASS ", Password]),
    send(Conn, ["NICK ", Nickname]),
    send(Conn, ["USER ", Nickname, " 8 * :", ?REALNAME]).

%%%
%%% gen_fsm states
%%%
standby(connect, StateData) ->
    {Host, Port} = StateData#state.server,
    Ssl = StateData#state.ssl,
    Pid = ircbot_connection:start_link(self(), Host, Port, Ssl),
    NewStateData = StateData#state{connection=Pid},
    io:format("connect in standby -> connecting~n"),
    {next_state, connecting, NewStateData, ?CONNECT_TIMEOUT};

standby({reconnect, How}, StateData) ->
    case How of
        fast ->
            Delay = ?RECONNECT_DELAY,
            Backoff = 0;
        backoff ->
            Backoff = if
                StateData#state.backoff >= 5 -> 5;
                true -> StateData#state.backoff + 1
            end,
            % some kind of quadratic backoff
            Delay = Backoff * Backoff * ?BACKOFF_DELAY + ?RECONNECT_DELAY
    end,
    io:format("reconnect in ~p seconds~n", [Delay/1000]),
    Ref = gen_fsm:send_event_after(Delay, connect),
    NewStateData = StateData#state{backoff=Backoff,timer=Ref},
    {next_state, standby, NewStateData};

standby(_Ev, StateData) ->
    {next_state, standby, StateData}.


connecting(timeout, StateData) ->
    gen_fsm:send_event_after(0, {reconnect, fast}),
    Pid = StateData#state.connection,
    erlang:exit(Pid, kill),
    io:format("timeout in connecting -> standby~n"),
    {next_state, standby, StateData};

connecting(exit, StateData) ->
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("connection died in connecting -> standby~n"),
    {next_state, standby, StateData};

connecting(success, StateData) ->
    Pid = StateData#state.connection,
    Nick = StateData#state.nickname,
    Password = StateData#state.password,
    send_login(Pid, Nick, Password),
    NewStateData = StateData#state{backoff=0},
    io:format("success in connecting -> registering~n"),
    {next_state, registering, NewStateData, ?REGISTER_TIMEOUT};

connecting(_Ev, StateData) ->
    {next_state, connecting, StateData, ?CONNECT_TIMEOUT}.



registering(timeout, StateData) ->
    Pid = StateData#state.connection,
    erlang:exit(Pid, kill),
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("timeout: register -> standby~n"),
    {next_state, standby, StateData};

registering(exit, StateData) ->
    gen_fsm:send_event_after(0, {reconnect, backoff}),
    io:format("connection died: register -> standby~n"),
    {next_state, standby, StateData};

registering({received, Msg}, StateData) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Msg),
    case IrcMessage of
        [_, _, <<"001">>, _, _] ->
            Self = ircbot_api:new(self()),
            Plugins = StateData#state.plugins,
            ircbot_plugins:notify(Plugins, {Self, online}),
            ircbot_plugins:notify(Plugins, {in, Self, IrcMessage}),
            {next_state, ready, StateData};
        [_, _, <<"433">>, <<"*">>, <<Nick/binary>>, _] ->
            ChangeNick = [<<"NICK ">>, Nick, ?NICK_SUFFIX],
            send(StateData#state.connection, ChangeNick),
            {next_state, registering, StateData, ?REGISTER_TIMEOUT};
        [_, _, <<"PING">>, Ping] ->
            Pong = [<<"PONG :", Ping/binary>>],
            send(StateData#state.connection, Pong),
            {next_state, registering, StateData, ?REGISTER_TIMEOUT};
        _ ->
            {next_state, registering, StateData, ?REGISTER_TIMEOUT}
    end;

registering(_Ev, StateData) ->
    {next_state, registering, StateData, ?REGISTER_TIMEOUT}.



ready({send, Msg}, StateData) ->
    send(StateData#state.connection, Msg),
    {next_state, ready, StateData};

ready({received, Msg}, StateData) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Msg),
    Self = ircbot_api:new(self()),
    Plugins = StateData#state.plugins,
    ircbot_plugins:notify(Plugins, {in, Self, IrcMessage}), % notify all plugins
    {next_state, ready, StateData};


ready(_Ev, StateData) ->
    {next_state, ready, StateData}.


handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
    % log Pid and Reason?
    io:format("Pid: ~p EXITed in state: ~p for reason: ~p~n", [Pid, StateName, Reason]),
    {stop, die, StateData};


handle_info(Info, StateName, StateData) ->
    %%% if StateName is connecting or registering should return a timeout
    io:format("BAD: ~p ~p~n", [Info, StateName]),
    {next_state, StateName, StateData}.

handle_event(_Ev, _StateName, _StateData) ->
    {stop, "Should never happen! Please don't use gen_fsm:send_all_state_event"}.


handle_sync_event(disconnect, _From, StateName, StateData) ->
    io:format("disconnect: ~p -> standby~n", [StateName]),
    Ref = StateData#state.timer,
    if
       is_reference(Ref) -> gen_fsm:cancel_timer(Ref);
       true -> ok
    end,
    Pid = StateData#state.connection,
    if
       is_pid(Pid) -> send_quit(Pid);
       true -> ok
    end,
    NewStateData = StateData#state{backoff=0,timer=undefined},
    {reply, ok, standby, NewStateData};

%% Plugin managemenet
handle_sync_event({add_plugin, Plugin, Args}, _From, StateName, StateData) ->
    Plugins = StateData#state.plugins,
    ircbot_plugins:add_handler(Plugins, Plugin, Args),
    {reply, ok, StateName, StateData};

handle_sync_event({delete_plugin, Plugin, Args}, _From, StateName, StateData) ->
    Plugins = StateData#state.plugins,
    ircbot_plugins:delete_handler(Plugins, Plugin, Args),
    {reply, ok, StateName, StateData};

handle_sync_event(which_plugins, _From, StateName, StateData) ->
    Plugins = StateData#state.plugins,
    Reply = ircbot_plugins:which_handlers(Plugins),
    {reply, Reply, StateName, StateData};

handle_sync_event(_Ev, _From, _StateName, _StateData) ->
    {stop, "Should never happen! Please don't use gen_fsm:sync_send_all_state_event"}.


%% OTP code_change and terminate
code_change(_OldVsn, StateName, StateData, _Extra) ->
    Pid = StateData#state.connection,
    if
       is_pid(Pid) -> Pid ! code_change;
       true -> ok
    end,
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.
