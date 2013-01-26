-module(plugins.prisutni).
-author("gorgi.kosev@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-import(ircbot_lib).

-import(ejson).
-import(proplists).
-import(hackney).

-define(MAXBODY, 10000).

init(_Args) ->
    hackney:start(),
    {ok, ok}.


handle_event(Msg, State) ->
    case Msg of
        % explicit command to fetch prisutni.spodeli.org
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!prisutni">>]} ->
            Url = <<"http://prisutni.spodeli.org/status?limit=1">>,
            Callback = fun(Answer) -> Ref:privmsg(Channel, Answer) end,
            spawn(fun() -> fetcher(Url, Callback) end),
            {ok, State};
       _ ->
            {ok, State}
    end.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.



%% The function gets spawned as a separate process,
%% and fails silently on many errors.
fetcher(Url, Callback) ->
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>}],
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, _RespHeaders, Client} = hackney:request(get, Url, Headers, <<>>, Options),
    {ok, Body, Client1} = hackney:body(?MAXBODY, Client),
    case StatusCode of
        200 ->
            {Json} = ejson:decode(Body),
            [{Counter}|_] = proplists:get_value(<<"counters">>, Json),
            Count = proplists:get_value(<<"count">>, Counter),
            CountS = integer_to_list(Count),
            People = proplists:get_value(<<"present">>, Json),

            case {Count, People} of
                {0, _} ->
                    Callback(<<"во хаклаб нема никој :(">>);
                {_, []} ->
                    Callback([<<"во хаклаб има ">>, CountS, <<" уреди">>]);
                _ ->
                    Names = [ proplists:get_value(<<"name">>, Person) || {Person} <- People ],
                    Callback([<<"Присутни: ">>, ircbot_lib:iolist_join(Names, ", "), <<". Вкупно уреди: ">>, CountS])
            end;
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            Callback(<<"{error ", N/binary, "}">>)
    end,
    hackney:close(Client1).
