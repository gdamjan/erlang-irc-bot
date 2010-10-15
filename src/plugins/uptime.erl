-module(plugins.uptime).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(calendar).
-import(io_lib).
-import(lists).
-import(proplists).
-import(erlang).


init(_Args) ->
    {ok, []}.

uptime() ->
    {UpTime, _ } = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).

memory() ->
    M = proplists:get_value(total, erlang:memory()),
    lists:flatten(io_lib:format("memory: ~p kb", [M / 1000])).

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!uptime">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [uptime(), " | ", memory()]),
            {ok, State};
        _ ->
            {ok, State}
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
