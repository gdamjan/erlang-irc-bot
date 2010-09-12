-module(ircbot_connection).
-author("gdamjan@gmail.com").

-include("ircbot.hrl").
-define(CRNL, "\r\n").

-export([start_link/3, code_switch/1, connect/3]).


start_link(Parent, Host, Port)  ->
    spawn_link(?MODULE, connect, [Parent, Host, Port]).

connect(Parent, Host, Port) ->
    connect(Parent, Host, Port, 0).

connect(Parent, Host, Port, Backoff) when Backoff > 5 ->
    connect(Parent, Host, Port, 5);

connect(Parent, Host, Port, Backoff) ->
    Options = [ binary, {active, true}, {packet, line}, {keepalive, true},
                {send_timeout, ?SEND_TIMEOUT}],
    case gen_tcp:connect(Host, Port, Options, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            Parent:send_event({connected, self()}),
            loop({Parent, Sock});
        {error, Reason} ->
            error_logger:format("Error connecting: ~s~n", [inet:format_error(Reason)]),
            timer:sleep(Backoff * Backoff * 5000),
            connect(Parent, Host, Port, Backoff + 1);
        Other ->
            error_logger:format("Error connecting: ~p~n", [Other])
    end.


loop({Parent, Sock} = State) ->
    receive
        code_switch ->
            ?MODULE:code_switch(State);

        % data to send away on the socket
        {send, Data} ->
            debug(out, [Data]), % for debuging only
            ok = gen_tcp:send(Sock, [Data, ?CRNL]),
            loop(State);

        % data received
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, ?CRNL), % strip the CRNL at the end
            debug(in, [Line]),    % for debuging only
            Parent:send_event({received, Line}),
            loop(State);

        % socket closed
        {tcp_closed, Sock} ->
            error_logger:format("Socket ~w closed [~w]~n", [Sock, self()]);

        % socket errors
        {tcp_error, Sock, Reason} ->
            error_logger:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]);

        % close socket and quit
        quit ->
            gen_tcp:close(Sock)

    after ?RECV_TIMEOUT ->
        error_logger:format("No activity for more than ~b microseconds. Are we stuck?~n", [?RECV_TIMEOUT]),
        gen_tcp:close(Sock)
    end.

code_switch(State) -> loop(State).

%% debug helpers
debug(in, Msg) ->
    ircbot_lib:debug([" IN| ", Msg]);

debug(out, Msg) ->
    ircbot_lib:debug(["OUT| ", Msg]).
