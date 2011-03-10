-module(ircbot_connection).
-author("gdamjan@gmail.com").

-include("ircbot.hrl").
-define(CRNL, "\r\n").

-export([start_link/3, code_change/1, connect/3]).


start_link(Parent, Host, Port)  ->
    spawn_link(?MODULE, connect, [Parent, Host, Port]).

connect(Parent, Host, Port) ->
    Options = [ binary, {active, true}, {packet, line}, {keepalive, true},
                {send_timeout, ?SEND_TIMEOUT}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Sock} ->
            gen_fsm:send_event(Parent, success),
            loop({Parent, Sock});
        {error, Reason} ->
            error_logger:format("gen_tcp:connect error: ~s~n", [inet:format_error(Reason)]);
        Other ->
            error_logger:format("gen_tcp:connect other error: ~p~n", [Other])
    end.


loop({Parent, Sock} = State) ->
    receive
        code_change ->
            ?MODULE:code_change(State);

        % data to send away on the socket
        {send, Data} ->
            debug(out, Data), % for debuging only
            ok = gen_tcp:send(Sock, [Data, ?CRNL]),
            loop(State);

        % data received
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, ?CRNL), % strip the CRNL at the end
            debug(in, Line),    % for debuging only
            gen_fsm:send_event(Parent, {received, Line}),
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

code_change(State) -> loop(State).

%% debug helpers
debug(in, Msg) ->
    io:put_chars(" IN| "),
    debug(Msg),
    io:put_chars("\n");

debug(out, Msg) ->
    io:put_chars("OUT| "),
    debug(Msg),
    io:put_chars("\n").

debug(Msg) ->
    case catch io:put_chars(Msg) of
        {'EXIT',{badarg, _}} ->
            io:write(iolist_to_binary(Msg));
        ok ->
            ok
    end.
