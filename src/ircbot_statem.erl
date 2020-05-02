-module(ircbot_statem).
-author("gdamjan@gmail.com").

-behaviour(gen_statem).

-export([start/1, start_link/1]).
-export([init/1, terminate/3, code_change/4, callback_mode/0]).
-export([standby/3, connecting/3, registering/3, ready/3, reconnect/3]).

-include("ircbot.hrl").


start(Settings) ->
    start_real(Settings, start).

start_link(Settings) ->
    start_real(Settings, start_link).

start_real(Settings, Fun) ->
    case proplists:get_value(name, Settings) of
        undefined ->
            gen_statem:Fun(?MODULE, Settings, []);
        FsmName ->
            gen_statem:Fun(FsmName, ?MODULE, Settings, [])
    end.

init(Settings) ->
    process_flag(trap_exit, true),
    Data = maps:from_list(Settings),
    {ok, Plugins} = ircbot_plugins:start_link(Settings),
    case maps:get(autoconnect, Data, false) of
        true -> gen_statem:cast(self(), connect);
        _ -> ok
    end,
    {ok, standby, Data#{plugins=>Plugins, reconnect_count=>0}}.


%%% Standby state - doing nothing, waiting fot the connect command/event
standby(enter, PrevState, #{connection := Pid} = Data) ->
    io:format("[standby] entered from ~p~n", [PrevState]),
    catch erlang:exit(Pid, kill),
    {keep_state, maps:remove(connection, Data)};

standby(enter, _, _Data) ->
    keep_state_and_data;

standby(cast, connect, Data) ->
    io:format("[standby] connecting…~n"),
    {next_state, connecting, Data};

standby(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%% Connecting state - creates the connection gen_server, if it fails, go to reconnect
connecting(enter, _, Data) ->
    {Host, Port} = maps:get(server, Data),
    Ssl = maps:get(ssl, Data, false),
    Pid = ircbot_connection:start_link(self(), Host, Port, Ssl),
    {keep_state, Data#{connection => Pid}, ?CONNECT_TIMEOUT};

connecting(timeout, _, Data) ->
    {next_state, reconnect, Data};

connecting(cast, connected, Data) ->
    io:format("[connecting] <- connected | => registering~n"),
    {next_state, registering, Data};

connecting(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%% Registering state - send login info, wait for 001 Welcome message, if it fails, go to reconnect
registering(enter, _PrevState, Data) ->
    #{connection:=Pid, nickname:=Nick } = Data,
    Password = maps:get(password, Data, undefined),
    send_login(Pid, Nick, Password),
    {keep_state, Data, ?REGISTER_TIMEOUT};

registering(timeout, _, Data) ->
    {next_state, reconnect, Data};

registering(cast, {received, Msg}, Data) ->
    #{connection:=Pid} = Data,
    {match, IrcMessage} = ircbot_lib:irc_parse(Msg),
    case IrcMessage of
        [_, _, <<"001">>, _, _] ->
            io:format("[registering] <<001 Welcome>> | => ready~n"),
            {next_state, ready, Data#{welcome=>IrcMessage}};
        [_, _, <<"433">>, <<"*">>, <<Nick/binary>>, _] ->
            ChangeNick = [<<"NICK ">>, Nick, ?NICK_SUFFIX],
            send(Pid, ChangeNick),
            {keep_state_and_data, ?REGISTER_TIMEOUT};
        [_, _, <<"PING">>, Ping] ->
            Pong = [<<"PONG :", Ping/binary>>],
            send(Pid, Pong),
            {keep_state_and_data, ?REGISTER_TIMEOUT};
        _ ->
            {keep_state_and_data, ?REGISTER_TIMEOUT}
    end;

registering(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%% Ready state - it's connected and registered - normal operation
ready(enter, _PrevState, Data) ->
    #{plugins:=Plugins, welcome:=Msg} = Data,
    Self = ircbot_api:new(self()),
    ircbot_plugins:notify(Plugins, {Self, online}),
    ircbot_plugins:notify(Plugins, {in, Self, Msg}),
    keep_state_and_data;

ready(cast, {send, Msg}, Data) ->
    #{connection:=Pid} = Data,
    send(Pid, Msg),
    keep_state_and_data;

ready(cast, {received, Msg}, Data) ->
    {match, IrcMessage} = ircbot_lib:irc_parse(Msg),
    #{plugins:=Plugins} = Data,
    Self = ircbot_api:new(self()),
    ircbot_plugins:notify(Plugins, {in, Self, IrcMessage}), % notify all plugins
    keep_state_and_data;

ready(cast, disconnect, Data) ->
    #{connection:=Pid} = Data,
    if
       is_pid(Pid) -> send_quit(Pid);
       true -> ok
    end,
    handle_event(cast, disconnect, Data);

ready(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%% Reconnect state - calculate the delay+backoff, wait, then go to standby, but schedule a connect event
calculate_backoff(Data) ->
    %%% quadratic backoff, capped to 10 times
    C = min(10, maps:get(reconnect_count, Data, 0)),
    Delay = C * C * ?BACKOFF_DELAY,
    Delay.

reconnect(enter, PrevState, Data) ->
    io:format("[reconnect] entered from ~p~n", [PrevState]),
    Timeout = calculate_backoff(Data),
    {keep_state, Data, {state_timeout, Timeout, timeout}};

reconnect(info, {'EXIT', Pid, killed}, #{connection:=Pid} = Data) ->
    io:format("[reconnect] connection killed ~p~n", [Pid]),
    Timeout = calculate_backoff(Data),
    {keep_state, Data, {state_timeout, Timeout, timeout}};

reconnect(state_timeout, _, Data) ->
    C = maps:get(reconnect_count, Data, 0) + 1,
    gen_statem:cast(self(), connect),
    {next_state, standby, Data#{reconnect_count=>C}};

reconnect(cast, connect, Data) ->
    gen_statem:cast(self(), connect),
    {next_state, standby, Data};

reconnect(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%%% Handle events common to all states
handle_event({call, From}, which_plugins, Data) ->
    #{plugins:=Plugins} = Data,
    Reply = ircbot_plugins:which_handlers(Plugins),
    {keep_state, Data, [{reply, From, Reply}]};

handle_event({call, From}, {add_plugin, Plugin, Args}, Data) ->
    #{plugins:=Plugins} = Data,
    ircbot_plugins:add_handler(Plugins, Plugin, Args),
    {keep_state, Data, [{reply, From, ok}]};

handle_event({call, From}, {delete_plugin, Plugin, Args}, Data) ->
    #{plugins:=Plugins} = Data,
    ircbot_plugins:delete_handler(Plugins, Plugin, Args),
    {keep_state, Data, [{reply, From, ok}]};

handle_event(cast, disconnect, Data) ->
    {next_state, standby, Data};

handle_event(info, {'EXIT', Pid, die}, #{connection:=Pid} = Data) ->
    io:format("[handle_event] connection died~n"),
    {next_state, reconnect, maps:remove(connection, Data)};

%%% Ignore all other events
handle_event(EventType, EventContent, Data) ->
    io:format("[ignore ~p] ~p~n~p~n", [EventType, EventContent, Data]),
    keep_state_and_data.


code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.
terminate(_Reason, _State, _Data) -> void.
callback_mode() ->
    [state_functions, state_enter].


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
