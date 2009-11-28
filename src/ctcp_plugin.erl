-module(ctcp_plugin).
-author("gdamjan@gmail.com").
-include_lib("common.hrl").

-export ([init/1,run/0]).


init(_Args) ->
    {ok, []}.

run() ->
    main_loop().

main_loop() ->
    receive
        {Pid, {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]}} ->
            Pid ! {send_data, ["NOTICE ", Sender, " :\^AVERSION ", ?VERSION, "\^A"]};
        {Pid, {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]}} ->
            Pid ! {send_data, ["NOTICE ", Sender, " :\^APING ", Rest]};
        _ ->
            ok
    end,
    main_loop().
