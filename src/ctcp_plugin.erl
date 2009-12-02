-module(ctcp_plugin).
-behaviour(gen_event).

-author("gdamjan@gmail.com").
-include_lib("common.hrl").

-export([init/1, handle_event/2, terminate/2]).


init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {Pid, {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]}} ->
            Pid ! {send_data, ["NOTICE ", Sender, " :\^AVERSION ", ?VERSION, "\^A"]};
        {Pid, {match, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]}} ->
            Pid ! {send_data, ["NOTICE ", Sender, " :\^APING ", Rest]};
        _ ->
            ok
    end,
    {ok, State}.

terminate(_Args, _State) ->
    ok.
