-module(plugins.hacklab_status).
-author("glisha").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-import(ircbot_lib).

-import(ejson).
-import(proplists).
-import(httpc).
-import(inets).


init(_Args) ->
    inets:start(),
    {ok, ok}.


handle_event(Msg, State) ->
    case Msg of
        % explicit command to fetch hacklab status from Cosm
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#lugola">>, <<"!status">>]} ->
            fetch("https://api.cosm.com/v2/feeds/86779/datastreams/hacklab_status.json", Ref, <<"#lugola">>),
            {ok, State};
       _ ->
            {ok, State}
    end.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.



% Fetch the json, but not more than 10kbytes
%% The function gets spawned as a separate process, and fails silently on any
%% error.
fetch(Url, Ref, Channel) ->
    F = fun(Answer) -> Ref:privmsg(Channel, Answer) end,
    spawn(fun() -> fetcher(Url, F) end).

fetcher(Url, Callback) ->
    Headers = [{"User-Agent", "Mozilla/5.0 (erlang-irc-bot)"},{"X-ApiKey","vqElqXeb7Lu6ZwDElnKQ8XpGMG-SAKxxMHV3YWFoeHE4OD0g"}],
    {ok, RequestId} = httpc:request(get, {Url, Headers}, [], [{sync, false}, {stream, self}]),
    receive_chunk(RequestId, Callback, [], 10000).

%% callback function called as chunks from http are received
%% when enough data is received (Len =< 0) process the json

receive_chunk(_RequestId, Callback, Body, Len) when Len =< 0 ->
    {Json} = ejson:decode(Body),
    Current_Value = proplists:get_value(<<"current_value">>, Json),

    case {Current_Value} of
        {0} ->
            Callback(<<"Хаклабот е затворен. :(">>);
        {1} ->
            Callback(<<"Хаклабот е отворен. Дојди! (http://status.spodeli.org)">>)
    end;

receive_chunk(RequestId, Callback, Body, Len)  ->
    receive
        {http,{RequestId, stream_start, _Headers}} ->
            receive_chunk(RequestId, Callback, Body, Len);

        {http,{RequestId, stream, Data}} ->
            Size = size(Data),
            receive_chunk(RequestId, Callback, Body ++ [Data], Len - Size);

        {http,{RequestId, stream_end, _Headers}} ->
            receive_chunk(RequestId, Callback, Body, 0)
    after 10000 ->
        ok
    end.

