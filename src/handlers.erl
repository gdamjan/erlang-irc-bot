-module(handlers).
-author('gdamjan@gmail.com').
-include_lib("common.hrl").

-export([init/0, process/1]).

init() ->
    ets:new(irclog_seen, [named_table]).

process(Line) ->
    case irc:parse(Line) of
        {match, [<<>>,<<>>,<<"PING">>, Server]} ->
            {respond, [<<"PONG :">>, Server]};
        {match, [Sender, _User, <<"JOIN">>, Channel]} ->
            do_join(Sender, Channel);
        {match, [Sender, _User, <<"QUIT">>, Message]} ->
            do_quit(Sender, Message);
        {match, [Sender, _User, <<"PART">>, Channel, Message]} ->
            do_part(Sender, Channel, Message);
        {match, [Sender, _User, <<"NICK">>, NewNick]} ->
            do_nick(Sender, NewNick);
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]} ->
            {respond, ["NOTICE ", Sender, " :\^AVERSION ", ?VERSION, "\^A"]};
        {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]} ->
            {respond, ["NOTICE ", Sender, " :\^APING ", Rest]};
        _ ->
            ok
    end.

do_join(Nick, Channel) ->
    ets:insert(irclog_seen, {{Nick, Channel}, {online, erlang:now()}}),
    ok.

do_part(Nick, Channel, Message) ->
    ets:insert(irclog_seen, {{Nick, Channel}, {offline, erlang:now(), Message}}),
    ok.

do_quit(Nick, Message) ->
    L = ets:match(irclog_seen, {{Nick, '$1'}, '_'}),
    lists:foreach(fun([Chan]) -> do_part(Nick, Chan, Message) end, L),
    ok.

do_nick(Nick, NewNick) ->
    Message = string:join(["changed nick to", NewNick], " "),
    L = ets:match(irclog_seen, {{Nick, '$1'}, '_'}),
    lists:foreach(fun([Chan]) ->
        do_part(Nick, Chan, Message),
        do_join(NewNick, Chan)
    end, L),
    ok.
