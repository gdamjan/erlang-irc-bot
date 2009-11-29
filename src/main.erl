-module(main).
-author('gdamjan@gmail.com').
-include_lib("common.hrl").

%% user interface
-export([start/1]).

%% exports for use within module only
-export([client/4, connect/5, code_switch/2]).


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
    case main_loop(Sock, []) of
        restart ->
            {Sleep, NextBackoff} = utils:backoff(Backoff),
            timer:sleep(Sleep),
            ?MODULE:connect(SomeHostInNet, Port, Nick, Channels, NextBackoff);
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
joinChannels(Sock, Channels) ->
    [ Channel| Rest ] = Channels,
    joinChannels(Sock, Channel, Rest).

joinChannels(Sock, Channel, []) ->
    send_msg(Sock, ["JOIN ", Channel]);

joinChannels(Sock, Channel, Channels) ->
    joinChannels(Sock, Channel, []),
    [ Channel_| Rest ] = Channels,
    joinChannels(Sock, Channel_, Rest).


send_msg(Sock, Message) ->
    io:format("OUT| ~ts~n", [Message]), % for debuging only
    gen_tcp:send(Sock, [Message, ?CRNL]).

ping(Sock, Server) ->
    send_msg(Sock, ["PING :", Server]).

% this is the main loop of the process, it will receive data from the socket
% and also messages from other processes, will loop forever until an unknown
% message is received.
main_loop(Sock, Plugins) ->
    receive
        % When the process receives this message, it will call 'code_switch/1'
        % from the *latest* MODULE version,
        % code_switch/1 just calls main_loop/1 again
        code_switch ->
            ?MODULE:code_switch(Sock, Plugins);
        quit ->
            io:format("Shuting down. "),
            quit(Sock),
            io:format("Bye.~n"),
            ok;
        restart ->
            quit(Sock),
            restart;
        ping ->
            ping(Sock, "irc.freenode.net"), % FIXME hardcoded server name
            main_loop(Sock, Plugins);

        {register_plugin, Module, InitArgs} ->
            {ok, RunArgs} = apply(Module, init, InitArgs),
            Pid = spawn(Module, run, RunArgs),
            io:format("Plugin '~s' registered.~n", [Module]),
            main_loop(Sock, [{Module, Pid} | Plugins]);
        {unregister_plugin, Module} ->
            case lists:keytake(Module, 1, Plugins) of
                {value, {Module, Pid}, NewPlugins} ->
                    exit(Pid, ok),
                    io:format("Plugin '~s' unregistered.~n", [Module]),
                    main_loop(Sock, NewPlugins);
                false ->
                    main_loop(Sock, Plugins)
            end;

        % message received from another process
        {send_data, Data} ->
            send_msg(Sock, Data),
            main_loop(Sock, Plugins);
        % data received from the socket
        {tcp, Sock, Data} ->
            [Line|_Tail] = re:split(Data, "\r\n"), % strip the CRNL at the end
            io:format(" IN| ~ts~n", [Line]),    % for debuging only, dies on leguin.freenode.net
            IrcMessage = irc:parse(Line),
            [Pid ! {self(), IrcMessage} || {_Name, Pid} <- Plugins], % notify all plugins
            main_loop(Sock, Plugins);

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
            main_loop(Sock, Plugins)

    after ?KEEPALIVE ->
        ping(Sock, "irc.freenode.net"),  % FIXME hardcoded server name
        main_loop(Sock, Plugins)
    end.

% when this function is called Erlang will have the chance to run a new
% main_loop(Sock, Plugins) implementation (see: Hot code reloading)
code_switch(Sock, Plugins) ->
    main_loop(Sock, Plugins).
