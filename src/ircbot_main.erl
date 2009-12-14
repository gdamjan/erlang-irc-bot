-module(ircbot_main).
-author('gdamjan@gmail.com').
-include_lib("ircbot.hrl").

%% user interface
-export([start/1]).

%% exports for use within module only
-export([connect/4, connect/5, code_switch/1]).


start(Args) ->
    gen_event:start({local, plugins}),
    gen_event:add_handler(plugins, pong_plugin, []), 
    gen_event:add_handler(plugins, ctcp_plugin, []), 
    spawn(?MODULE, connect, Args).

connect(SomeHostInNet, Port, Nick, Channels) ->
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
            ?MODULE:connect(SomeHostInNet, Port, Nick, Channels, NextBackoff);
        _ ->
            ok
    end.

login(Host, Port, Nick, Channels) ->
    {ok, Sock} = gen_tcp:connect(Host, Port,
                    [binary, {active, true}, {packet, line}], ?TCPTIMEOUT),
    registerNick(Sock, Nick),
    joinChannels(Sock, Channels),
    {ok, Sock}.

quit(Sock) ->
     send_msg(Sock, ["QUIT :", ?QUITMSG]),
     gen_tcp:close(Sock).

registerNick(Sock, Nick) ->
    % Connection Registration:
    % on freenode you must fire these very soon after connecting or the server
    % disconnects you
    send_msg(Sock, ["NICK ", Nick]),
    send_msg(Sock, ["USER ", Nick, " 8 * :", ?REALNAME]).

% recurses through the list 'Channels' and JOINs each of them
% no error checking!
joinChannels(_Sock, []) ->
    ok;

joinChannels(Sock, [ C | Rest ]) ->
    send_msg(Sock, ["JOIN ", C]),
    joinChannels(Sock, Rest).


send_msg(Sock, Message) ->
    debug(out, [Message]), % for debuging only
    gen_tcp:send(Sock, [Message, ?CRNL]).

debug(in, Msg) ->
    utils:debug([" IN| ", Msg]);

debug(out, Msg) ->
    utils:debug(["OUT| ", Msg]).

% this is the main loop of the process, it will receive data from the socket
% and also messages from other processes, will loop forever until an unknown
% message is received.
main_loop(Sock) ->
    receive
        % When the process receives this message, it will call 'code_switch/1'
        % from the *latest* MODULE version,
        % code_switch/1 just calls main_loop/1 again
        code_switch ->
            ?MODULE:code_switch(Sock);
        quit ->
            io:format("Shuting down. "),
            quit(Sock),
            io:format("Bye.~n"),
            ok;
        restart ->
            quit(Sock),
            restart;
        ping ->
            gen_event:notify(plugins, {self(), keepalive}),
            main_loop(Sock);

        % message received from another process
        {send_data, Data} ->
            send_msg(Sock, Data),
            main_loop(Sock);
        % data received from the socket
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, "\r\n"), % strip the CRNL at the end
            debug(in, [Line]),    % for debuging only, dies on leguin.freenode.net
            IrcMessage = utils:irc_parse(Line),
            gen_event:notify(plugins, {self(), IrcMessage}), % notify all plugins
            main_loop(Sock);

        % handle errors on the socket
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

    after ?KEEPALIVE ->
        gen_event:notify(plugins, {self(), keepalive}),
        main_loop(Sock)
    end.

% when this function is called Erlang will have the chance to run a new
% main_loop(Sock) implementation (see: Hot code reloading)
code_switch(Sock) ->
    main_loop(Sock).
