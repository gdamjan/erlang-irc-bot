-module(plugins.google).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(ircbot_lib).
-import(proplists).
-import(http).
-import(inets).

init(_Args) ->
    inets:start(),
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!g ", Query/binary>>]} ->
            fetch(Query, Ref, <<"#",Channel/binary>>),
            {ok, State};
         {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!google ", Query/binary>>]} ->
            fetch(Query, Ref, <<"#",Channel/binary>>),
            {ok, State};
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.


fetch(Query, Ref, Channel) ->
    Callback = fun(Url) -> Ref:privmsg(Channel, Url) end,
    spawn(fun() -> gfl(Query, Callback) end).

gfl(Query, Callback) ->
    Headers = [{"User-Agent", "Mozilla/5.0 (erlang-irc-bot)"}],
    Q = ircbot_lib:escape_uri(Query),
    Url = "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=" ++ Q,
    case http:request(get, {Url, Headers}, [{autoredirect, false}], []) of
        {ok, {{_,302,_}, ResponseHeaders, _}} ->
            Callback(proplists:get_value("location", ResponseHeaders));
        {ok, {{_,200,_}, _, _}} ->
            Callback(["No match, see: ", Url]);
        _ ->
            Callback("search error")
    end.
