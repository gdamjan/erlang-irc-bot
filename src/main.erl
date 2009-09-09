-module(main).
-author('gdamjan@gmail.com').

-export([start/1, client/4]).

-define(REALNAME, "Damjan's experimental Erlang IRC bot").
-define(CRNL, "\r\n").

start(Args) ->
    spawn(?MODULE, client, Args).

client(SomeHostInNet, Port, Nick, Channels) ->
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port,
                    [binary, {active, true}, {packet, line}]),
    % must fire these very soon after connecting
    gen_tcp:send(Sock, ["NICK ", Nick, ?CRNL]), 
    gen_tcp:send(Sock, ["USER ", Nick, " 0 *  : ", ?REALNAME, ?CRNL]),
    joinChannels(Sock, Channels),
    main_loop(Sock).

joinChannels(Sock, Channels) ->
    [ Channel| Rest ] = Channels,
    joinChannels(Sock, Channel, Rest).

joinChannels(Sock, Channel, []) ->
    gen_tcp:send(Sock, ["JOIN ", Channel, ?CRNL]);

joinChannels(Sock, Channel, Channels) ->
    joinChannels(Sock, Channel, []),
    [ Channel_| Rest ] = Channels,
    joinChannels(Sock, Channel_, Rest).

main_loop(Sock) ->
    receive
        {Client, send_data, Binary} ->
            case gen_tcp:send(Sock, [Binary]) of
                ok ->
                    Client ! {self(), data_sent},
                    main_loop(Sock)
            end;
        {tcp, Sock, Line} ->
            case process(Line) of
                ok ->
                    ok;
                Answer ->
                    gen_tcp:send(Sock, Answer)
            end,
            main_loop(Sock);
        {tcp_closed, Sock} ->
            io:format("Socket ~w closed [~w]~n", [Sock, self()]),
            ok;
        Other ->
            io:format("Got ~w - goodbye!~n", [Other]),
            gen_tcp:send(Sock, ["QUIT : erlang sucks - just kidding :)", ?CRNL]),
            gen_tcp:close(Sock),
            ok
    end.

process(Line) ->
    case Line of
        <<"PING", Rest/binary>> ->
            [<<"PONG">>, Rest];
        Else ->
            io:format("~ts", [Else]),
            ok
    end.
