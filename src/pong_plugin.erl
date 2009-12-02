-module(pong_plugin).
-behaviour(gen_event).

-author("gdamjan@gmail.com").

-export([init/1, handle_event/2, terminate/2]).


init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {_Pid, {match, [Server, _, <<"001">>, _Nick, _]}} ->
            {ok, Server};
        {Pid, {match, [<<>>,<<>>,<<"PING">>, Server]}} ->
            Pid ! {send_data, [<<"PONG :">>, Server]},
            {ok, Server};
        {Pid, keepalive} ->
            Pid ! {send_data, ["PING :", State]},
            {ok, State};
        _ ->
            {ok, State}
    end.

terminate(_Args, _State) ->
    ok.
