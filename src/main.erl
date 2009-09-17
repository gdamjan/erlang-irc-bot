-module(main).
-author('gdamjan@gmail.com').
-include_lib("common.hrl").

%% user interface
-export([start/1]).

%% exports for use within module only (used in apply, spawn)
-export([client/4, codeswitch/1]).


start(Args) ->
    spawn(?MODULE, client, Args).

client(SomeHostInNet, Port, Nick, Channels) ->
    connect(SomeHostInNet, Port, Nick, Channels, 1).

connect(SomeHostInNet, Port, Nick, Channels, Backoff) ->
    % open a TCP connection to the IRC server, we set the socket options to
    % {packet, line} which means will receive data line-by-line (which is very
    % neat for the IRC protocol).
    % FIXME: error handling
    {ok, Sock} = login(SomeHostInNet, Port, Nick, Channels),
    case main_loop(Sock) of
        restart ->
            {Sleep, NextBackoff} = utils:backoff(Backoff),
            timer:sleep(Sleep),
            connect(SomeHostInNet, Port, Nick, Channels, NextBackoff);
        _ ->
            ok
    end.

login(SomeHostInNet, Port, Nick, Channels) ->
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port,
                    [binary, {active, true}, {packet, line}], ?TCPTIMEOUT),
    registerNick(Sock, Nick),
    joinChannels(Sock, Channels),
    {ok, Sock}.

quit(Sock) ->
     gen_tcp:send(Sock, ["QUIT :", ?QUITMSG, ?CRNL]),
     gen_tcp:close(Sock).

registerNick(Sock, Nick) ->
    % Connection Registration:
    % on freenode you must fire these very soon after connecting or the server
    % disconnects you
    gen_tcp:send(Sock, ["NICK ", Nick, ?CRNL]),
    gen_tcp:send(Sock, ["USER ", Nick, " 0 *  : ", ?REALNAME, ?CRNL]).

% recurses through the list 'Channels' and JOINs each of them
% no error checking!
joinChannels(Sock, Channels) ->
    [ Channel| Rest ] = Channels,
    joinChannels(Sock, Channel, Rest).

joinChannels(Sock, Channel, []) ->
    gen_tcp:send(Sock, ["JOIN ", Channel, ?CRNL]);

joinChannels(Sock, Channel, Channels) ->
    joinChannels(Sock, Channel, []),
    [ Channel_| Rest ] = Channels,
    joinChannels(Sock, Channel_, Rest).


% this is the main loop of the process, it will receive data from the socket
% and also messages from other processes, will loop forever until an unknown
% message is received.
main_loop(Sock) ->
    receive
        % When the process receives this message, it will call 'codeswitch/1'
        % from the *latest* MODULE version,
        % codeswitch/1 just calls main_loop/1 again
        code_switch ->
            ?MODULE:codeswitch(Sock);
        quit ->
            io:format("Shuting down. "),
            quit(Sock),
            io:format("Bye.~n"),
            ok;
        restart ->
            quit(Sock),
            restart;
        % message received from another process
        {Client, send_data, Data} ->
            case gen_tcp:send(Sock, [Data, ?CRNL]) of
                ok ->
                    Client ! {self(), data_sent},
                    main_loop(Sock)
            end;
        % data received from the socket
        {tcp, Sock, Data} ->
            [Line, _] = re:split(Data, "\r\n"), % strip the CRNL at the end
            io:format(" IN: ~ts~n", [Line]),    % for debuging only
            case handlers:process(Line) of
                {respond, Response} ->
                    io:format("OUT: ~ts~n", [Response]), % for debuging only
                    gen_tcp:send(Sock, [Response, ?CRNL]);
                _ ->
                    ok
            end,
            main_loop(Sock);
        % FIXME: handle errors on the socket
        {tcp_error, Sock, Reason} ->
            io:format("Socket ~w error: ~w [~w]~n", [Sock, Reason, self()]),
            restart;
        {tcp_closed, Sock} ->
            io:format("Socket ~w closed [~w]~n", [Sock, self()]),
            restart;
        % catch all, log and loop back
        CatchAll ->
            io:format("UNK: ~w~n", [CatchAll]),
            main_loop(Sock)
    end.

% when this function is called Erlang will have the chance to run a new
% main_loop(Sock) implementation (see: Hot code reloading)
codeswitch(Sock) ->
    main_loop(Sock).
