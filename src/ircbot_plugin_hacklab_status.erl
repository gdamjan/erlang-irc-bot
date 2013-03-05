-module(plugins.hacklab_status).
-author("glisha").

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
        % explicit command to fetch hacklab status from Cosm
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, Channel = <<"#lugola">>, <<"!status">>]} ->
            Url = <<"https://api.cosm.com/v2/feeds/86779/datastreams/hacklab_status.json">>,
            F = fun(Answer) -> Ref:privmsg(Channel, Answer) end,
            spawn(fun() -> fetcher(Url, F) end),
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
    Headers = [{<<"User-Agent">>, <<"Mozilla/5.0 (erlang-irc-bot)">>},
                {<<"X-ApiKey">>,<<"vqElqXeb7Lu6ZwDElnKQ8XpGMG-SAKxxMHV3YWFoeHE4OD0g">>}],
    Options = [{recv_timeout, 5000}, {follow_redirect, true}],
    {ok, StatusCode, _RespHeaders, Client} = hackney:request(get, Url, Headers, <<>>, Options),
    {ok, Body, Client1} = hackney:body(?MAXBODY, Client),
    case StatusCode of
        200 ->
            {Json} = ejson:decode(Body),
            Current_Value = proplists:get_value(<<"current_value">>, Json),
            case Current_Value of
                <<"0">> ->
                    Callback(<<"Хаклабот е затворен. :(">>);
                <<"1">> ->
                    Callback(<<"Хаклабот е отворен. Дојди! (http://status.spodeli.org)">>)
            end;
        _ ->
            N = list_to_binary(integer_to_list(StatusCode)),
            Callback(<<"{error ", N/binary, "}">>)
    end,
    hackney:close(Client1).
