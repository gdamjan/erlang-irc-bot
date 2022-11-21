-module(ircbot_connection).
-author("gdamjan@gmail.com").

-include("ircbot.hrl").

-export([start_link/4, code_change/1, connect/3, connect/4]).


start_link(Parent, Host, Port, Ssl)  ->
    spawn_link(?MODULE, connect, [Parent, Host, Port, Ssl]).

connect(Parent, Host, Port) ->
    connect(Parent, Host, Port, false).

connect(Parent, Host, Port, Ssl) ->
    Options = [ binary, {active, true}, {packet, line}, {keepalive, true},
                {send_timeout, ?SEND_TIMEOUT}],
    SocketType = case Ssl of
                     true ->
                         ssl:start(),
                         ssl;
                     false ->
                         gen_tcp
                 end,
    case SocketType:connect(Host, Port, Options) of
        {ok, Sock} ->
            gen_fsm:send_event(Parent, success),
            loop({Parent, Sock, SocketType});
        {error, Reason} ->
            error_logger:format("gen_tcp:connect error: ~s~n", [inet:format_error(Reason)])
    end,
    exit(die).


loop({_, Sock, SocketType} = State) ->
    receive
        code_change ->
            ?MODULE:code_change(State);

        % data to send away on the socket
        {send, Data} ->
            logger:debug("OUT| ~ts", [Data]),
            ok = SocketType:send(Sock, [Data, ?CRNL]),
            loop(State);

        % data received - packet line makes sure it's a single line
        {tcp, Sock, Line} ->
            handle_recv_data(State, Line),
            loop(State);

        {ssl, Sock, Line} ->
            handle_recv_data(State, Line),
            loop(State);

        % socket closed
        {tcp_closed, Sock} ->
            handle_closed(Sock);

        {ssl_closed, Sock} ->
            handle_closed(Sock);

        % socket errors
        {tcp_error, Sock, Reason} ->
            handle_error(Sock, Reason);

        {ssl_error, Sock, Reason} ->
            handle_error(Sock, Reason);

        % close socket and quit
        quit ->
            SocketType:close(Sock)

    after ?RECV_TIMEOUT ->
            error_logger:format("No activity for more than ~b microseconds. Are we stuck?~n", [?RECV_TIMEOUT]),
            SocketType:close(Sock)
    end.

handle_recv_data({Parent, _, _}, LineIn) ->
    Line = string:chomp(LineIn),
    logger:debug(" IN| ~ts", [Line]),
    gen_fsm:send_event(Parent, {received, Line}).


handle_closed(Sock) ->
    error_logger:format("Socket ~w closed [~w]~n", [Sock, self()]).

handle_error(Sock, Reason) ->
    error_logger:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]).

code_change(State) -> loop(State).
