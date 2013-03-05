-module(ircbot_plugin_title).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).



-define(MAXBODY, 10000).

init(_Args) ->
    hackney:start(),
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
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, RespHeaders, Client} = hackney:request(get, Url1, Headers, <<>>, Options),
    case StatusCode of
        200 ->
            <<"text/", _/binary>> = hackney_headers:get_value(<<"content-type">>, hackney_headers:new(RespHeaders)),
            {ok, Body, Client1} = hackney:body(?MAXBODY, Client),
            Tree = mochiweb_html:parse(Body),
            [{_, _, Title}|_] = mochiweb_xpath:execute("//title",Tree),
            Title1 = re:replace(Title, "\\s+", " ", [global]),
            Callback(Title1),
            hackney:close(Client1);
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            Callback(<<"{error ", N/binary, "}">>),
            hackney:close(Client)
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
