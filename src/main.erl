-module(main).

-export([start/1, client/4]).

start(Args) ->
    spawn(?MODULE, client, Args).

client(SomeHostInNet, Port, Nick, Channels) ->
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port,
                    [binary, {active, true}, {packet, 0}]),
    % must fire these very soon after connecting
    gen_tcp:send(Sock, string:join(["NICK", Nick, "\r\n"], " ")), 
    gen_tcp:send(Sock, string:join(["USER", Nick, "0 *  :", Nick, "\r\n"], " ")),
    joinChannels(Sock, Channels),
    main_loop(Sock).

joinChannels(Sock, Channels) ->
    [ Channel| Rest ] = Channels,
    joinChannels(Sock, Channel, Rest).

joinChannels(Sock, Channel, []) ->
    gen_tcp:send(Sock, string:join(["JOIN", Channel, "\r\n"], " "));

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
        {tcp, Sock, Data} ->
            case process(Data) of
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
            gen_tcp:send(Sock, "QUIT : erlang sucks - just kidding :)\r\n"),
            gen_tcp:close(Sock),
            ok
    end.

process(Data) ->
    io:format("~ts", [Data]),
    ok.
