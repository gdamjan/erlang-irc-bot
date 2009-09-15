-module(handlers).
-author('gdamjan@gmail.com').

-export([process/1]).

process(Line) ->
    case irc:parse(Line) of
        {match, [<<>>,<<>>,<<"PING">>, Server]} ->
            {respond, [<<"PONG :">>, Server]};
        _ ->
            ok
    end.
