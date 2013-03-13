-module(plugins.title).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).




init(_Args) ->
    inets:start(),
    ssl:start(),
    {ok, dict:new()}.

handle_event(Msg, State) ->
    case Msg of
        % explicit command to fetch a web page title
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!t ", Url/binary>>]} ->
            fetch(Url, Ref, <<"#",Channel/binary>>),
            {ok, State};
         {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!title ", Url/binary>>]} ->
            fetch(Url, Ref, <<"#",Channel/binary>>),
            {ok, State};
        % fetch the title of the last url that appeared on the channel
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!t">>]} ->
            NewState = fetch_last(State, Ref, <<"#",Channel/binary>>),
            {ok, NewState};
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!title">>]} ->
            NewState = fetch_last(State, Ref, <<"#",Channel/binary>>),
            {ok, NewState};
        % look if there's an url in the text message on the channel, and
        % remmember it
        {in, _Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Text]} ->
            case ircbot_lib:url_match(Text) of
                {match, [Url]} ->
                    {ok, dict:store(<<"#",Channel/binary>>, Url, State)};
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

fetch_last(State, Ref, Channel) ->
    case dict:is_key(Channel, State) of
        true ->
            Url = dict:fetch(Channel, State),
            fetch(Url, Ref, Channel),
            dict:erase(Channel, State);
        false ->
            State
    end.


%% Fetch the url and find it's <title/>, but not more than 10kbytes and nothing
%% that isn't content-type: text/*
%% The function gets spawned as a separate process, and fails silently on any
%% error.
fetch(Url, Ref, Channel) ->
    F = fun(Title) -> Ref:privmsg(Channel, Title) end,
    spawn(fun() -> fetcher(Url, F) end).

fetcher(Url, Callback) ->
    Url1 = sanitize_url(Url),
    Headers = [{"User-Agent", "Mozilla/5.0 (erlang-irc-bot)"}],
    {ok, RequestId} = httpc:request(get, {Url1, Headers}, [], [{sync, false}, {stream, self}]),
    receive_chunk(RequestId, Callback, [], 10000).

%% callback function called as chunks from http are received
%% when enough data is received (Len =< 0) process the info

receive_chunk(_RequestId, Callback, Body, Len) when Len =< 0 ->
    {match, [Title]} = re:run(Body, "<title.*?>([\\s\\S]*?)</title>", [caseless, {capture, [1], binary}]),
    NewTitle = re:replace(Title, "\\s+", " ", [global]),
    % MAYBE recode charset to UTF-8
    Callback(NewTitle);

receive_chunk(RequestId, Callback, Body, Len) ->
    receive
        {http,{RequestId, stream_start, Headers}} ->
            ContentType = proplists:get_value("content-type", Headers, ""),
            % this will fail if the content-type is not text/*
            true = lists:prefix("text/", ContentType),
            receive_chunk(RequestId, Callback, Body, Len);

        {http,{RequestId, stream, Data}} ->
            Size = size(Data),
            receive_chunk(RequestId, Callback, Body ++ [Data], Len - Size);

        {http,{RequestId, stream_end, Headers}} ->
            ContentType = proplists:get_value("content-type", Headers, ""),
            true = lists:prefix("text/", ContentType),
            receive_chunk(RequestId, Callback, Body, 0)
    after 10000 ->
        ok
    end.


sanitize_url(Url) when is_binary(Url) ->
    sanitize_url(erlang:binary_to_list(Url));

sanitize_url(Url) when is_list(Url) ->
    Url1 = string:strip(Url),
    case {lists:prefix("http://", Url1),lists:prefix("https://", Url1)} of
        {false, false} ->
            lists:append("http://", Url1);
        _ ->
            Url1
    end.
