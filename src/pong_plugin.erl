-module(pong_plugin).
-author("gdamjan@gmail.com").
-include_lib("common.hrl").

-export ([init/1,run/0]).


init(_Args) ->
    {ok, []}.

run() ->
    main_loop().

main_loop() ->
    receive
        {Pid, {match, [<<>>,<<>>,<<"PING">>, Server]}} ->
            Pid ! {send_data, [<<"PONG :">>, Server]};
        _ ->
            ok
    end,
    main_loop().
