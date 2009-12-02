-module(pong_plugin).
-behaviour(gen_event).

-author("gdamjan@gmail.com").
-include_lib("common.hrl").

-export([init/1, handle_event/2, terminate/2]).


init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {Pid, {match, [<<>>,<<>>,<<"PING">>, Server]}} ->
            Pid ! {send_data, [<<"PONG :">>, Server]};
        _ ->
            ok
    end,
    {ok, State}.

terminate(_Args, _State) ->
    ok.
