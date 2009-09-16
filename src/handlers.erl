-module(handlers).
-author('gdamjan@gmail.com').

-export([process/1]).

process(Line) ->
    case irc:parse(Line) of
        {match, [<<>>,<<>>,<<"PING">>, Server]} ->
            {respond, [<<"PONG :">>, Server]};
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]} ->
            {respond, ["NOTICE ", Sender, " :\^AVERSION erlbot-0.1\^A"]}; 
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]} ->
            {respond, ["NOTICE ", Sender, " :\^APING ", Rest]};
        _ ->
            ok
    end.
