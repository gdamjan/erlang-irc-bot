-module(plugins.title).
-behaviour(gen_event).

-author("gdamjan@gmail.com").

-import(http).
-import(re).
-import(lists).
-import(dict).
-import(inets).
-export([init/1, handle_event/2, terminate/2, getter/3]).


init(_Args) ->
    inets:start(),
    {ok, dict:new()}.

handle_event(Msg, State) ->
    case Msg of
        {Pid, {match, [_Nick, _Name, <<"PRIVMSG">>, Channel, <<"!title ", Url/binary>>]}} ->
            spawn(?MODULE, getter, [Url, Pid, Channel]),
            {ok, State};
        {Pid, {match, [_Nick, _Name, <<"PRIVMSG">>, Channel, <<"!title">>]}} ->
            case dict:is_key(Channel, State) of
                true -> 
                    Url = dict:fetch(Channel, State),
                    spawn(?MODULE, getter, [Url, Pid, Channel]),
                    {ok, dict:erase(Channel, State)};
                false ->
                    {ok, State}
            end;
        {_Pid, {match, [_Nick, _Name, <<"PRIVMSG">>, Channel, Text]}} ->
            case re:run(Text, "\\b((http://|www\\.)[\\S]*)\\b", [caseless, {capture, [1], binary}]) of
                {match, [Url]} ->
                    {ok, dict:store(Channel, Url, State)};
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

terminate(_Args, _State) ->
    ok.

sanitize_url(Url) when is_binary(Url) ->
    sanitize_url(erlang:binary_to_list(Url));

sanitize_url(Url) when is_list(Url) ->
    case lists:prefix("http://", Url) of
        true -> Url;
        false -> lists:append("http://", Url)
    end.

getter(Url, Pid, Channel) ->
    {ok, {_Status, _Headers, Body}} = http:request(sanitize_url(Url)),
    {match, [Title]} = re:run(Body, "<title.*>([\\s\\S]*)</title>", [caseless, {capture, [1], binary}]),
    NewTitle = re:replace(Title, "\\s+", " ", [global]),
    Pid ! {send_data, ["PRIVMSG ", Channel, " :", NewTitle]}.
