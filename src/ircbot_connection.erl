-module(ircbot_connection).
-author('gdamjan@gmail.com').

-include_lib("ircbot.hrl").
-define(CRNL, "\r\n").

-export([start_link/3]).


start_link(Parent, Host, Port)  ->
    spawn_link(fun () -> connect(Parent, Host, Port, 0) end).

connect(Parent, Host, Port, Backoff) when Backoff > 5 ->
    connect(Parent, Host, Port, 5);

connect(Parent, Host, Port, Backoff) ->
    Opts = [ binary, {active, true}, {packet, line}, {keepalive, true},
                {send_timeout, ?SEND_TIMEOUT}],
    case gen_tcp:connect(Host, Port, Opts, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            loop(Parent, Sock);
        _  ->
            timer:sleep(Backoff * Backoff * 5000),
            connect(Parent, Host, Port, Backoff + 1)
    end.


loop(Parent, Sock) ->
    receive
        % data to send away on the socket
        {send_data, Data} ->
            debug(out, [Data]), % for debuging only
            gen_tcp:send(Sock, [Data, ?CRNL]),
            loop(Parent, Sock);

        % data received from the socket
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, ?CRNL), % strip the CRNL at the end
            debug(in, [Line]),    % for debuging only
            gen_server:cast(Parent, {received_data, Line}),
            loop(Parent, Sock);

        % close socket and quit
        quit ->
            gen_tcp:close(Sock);

        % handle errors on the socket
        {tcp_error, Sock, Reason} ->
            io:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]);

        {tcp_closed, Sock} ->
            io:format("Socket ~w closed [~w]~n", [Sock, self()])
    end.


%% debug helpers
debug(in, Msg) ->
    utils:debug([" IN| ", Msg]);

debug(out, Msg) ->
    utils:debug(["OUT| ", Msg]).
