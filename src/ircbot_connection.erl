-module(ircbot_connection).
-author("gdamjan@gmail.com").

-include("ircbot.hrl").
-define(CRNL, "\r\n").

-export([start_link/4, code_change/1, connect/3, connect/4]).


start_link(Parent, Host, Port, Ssl)  ->
    spawn_link(?MODULE, connect, [Parent, Host, Port, Ssl]).

connect(Parent, Host, Port) ->
    connect(Parent, Host, Port, false).

connect(Parent, Host, Port, Ssl) ->
    ircbot_log:init(),
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
            ircbot_log:debug(out, Data), % for debuging only
            ok = SocketType:send(Sock, [Data, ?CRNL]),
            loop(State);

        % data received
        {tcp, Sock, Data} ->
            handle_recv_data(State, Data),
            loop(State);

        {ssl, Sock, Data} ->
            handle_recv_data(State, Data),
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

code_change(State) -> loop(State).

handle_recv_data({Parent, _, _}, Data) ->
    [Line|_Tail] = re:split(Data, ?CRNL), % strip the CRNL at the end
    ircbot_log:debug(in, Line),    % for debuging only
    gen_fsm:send_event(Parent, {received, Line}).

handle_closed(Sock) ->
    error_logger:format("Socket ~w closed [~w]~n", [Sock, self()]).

handle_error(Sock, Reason) ->
    error_logger:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]).
