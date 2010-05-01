-module(ircbot_connection).
-author('gdamjan@gmail.com').

-include_lib("ircbot.hrl").
-define(CRNL, "\r\n").

-export([start_link/3, code_switch/1]).


start_link(Parent, Host, Port)  ->
    spawn_link(fun () -> connect(Parent, Host, Port, 0) end).

connect(Parent, Host, Port, Backoff) when Backoff > 5 ->
    connect(Parent, Host, Port, 5);

connect(Parent, Host, Port, Backoff) ->
    Opts = [ binary, {active, true}, {packet, line}, {keepalive, true},
                {send_timeout, ?SEND_TIMEOUT}],
    utils:debug(["Connecting..."]),
    case gen_tcp:connect(Host, Port, Opts, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            utils:debug(["Connection established!"]),
            loop({Parent, Sock});
        {error, Reason}  ->
            utils:debug(["Error connecting: ", inet:format_error(Reason)]),
            timer:sleep(Backoff * Backoff * 5000),
            connect(Parent, Host, Port, Backoff + 1);
        Other ->
            io:format("~p~n", [Other])
    end.


loop({Parent, Sock} = State) ->
    receive
        % data to send away on the socket
        {send_data, Data} ->
            debug(out, [Data]), % for debuging only
            gen_tcp:send(Sock, [Data, ?CRNL]),
            loop(State);

        % data received from the socket
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, ?CRNL), % strip the CRNL at the end
            debug(in, [Line]),    % for debuging only
            gen_server:cast(Parent, {received_data, Line}),
            loop(State);

        % close socket and quit
        quit ->
            gen_tcp:close(Sock);

        % Force the use of 'codeswitch/1' from the latest MODULE version
        code_switch ->
            ?MODULE:code_switch(State);

        % handle errors on the socket
        {tcp_error, Sock, Reason} ->
            io:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]);

        {tcp_closed, Sock} ->
            io:format("Socket ~w closed [~w]~n", [Sock, self()])
    end.

code_switch(State) -> loop(State).

%% debug helpers
debug(in, Msg) ->
    utils:debug([" IN| ", Msg]);

debug(out, Msg) ->
    utils:debug(["OUT| ", Msg]).
