-module(handlers).
-author('gdamjan@gmail.com').
-include_lib("common.hrl").

-export([process/1]).


process(Line) ->
    case irc:parse(Line) of
        {match, [<<>>,<<>>,<<"PING">>, Server]} ->
            {respond, [<<"PONG :">>, Server]};
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]} ->
            {respond, ["NOTICE ", Sender, " :\^AVERSION ", ?VERSION, "\^A"]}; 
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]} ->
            {respond, ["NOTICE ", Sender, " :\^APING ", Rest]};
        _ ->
            ok
    end.
