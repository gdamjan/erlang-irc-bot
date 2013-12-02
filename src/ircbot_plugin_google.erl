-module(ircbot_plugin_google).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


init(_Args) ->
    hackney:start(),
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
    Callback = fun(Msg) -> Ref:privmsg(Channel, Msg) end,
    spawn(fun() -> gfl(Query, Callback) end).

gfl(Query, Callback) ->
    Q = hackney_url:urlencode(Query),
    Url = <<"http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=", Q/binary>>,
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 5000}],
    {ok, StatusCode, RespHeaders, Ref} = hackney:request(get, Url, Headers, <<>>, Options),
    case StatusCode of
        302 ->
            LuckyResult =  hackney_headers:get_value(<<"location">>, hackney_headers:new(RespHeaders)),
            Callback(LuckyResult),
            hackney:close(Ref);
        200 ->
            Callback(["No match, see: ", Url]),
            hackney:close(Ref);
        _ ->
            ok
    end.
