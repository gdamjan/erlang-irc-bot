-module(ctcp_plugin).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-include("ircbot.hrl").


init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^AVERSION\^A">>]} ->
            Ref:notice(Sender, ["\^AVERSION ", ?VERSION, "\^A"]);
        {in, Ref, [Sender, _User, <<"PRIVMSG">>, _Nick, <<"\^APING ", Rest/binary>>]} ->
            Ref:notice(Sender, ["\^APING ", Rest]);
        _ ->
            ok
    end,
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
