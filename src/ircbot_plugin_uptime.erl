-module(ircbot_plugin_uptime).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).



init(_Args) ->
    {ok, []}.

iff0(N, Suffix) ->
    iff0(N, <<"">>, Suffix).

iff0(N, Prefix, Suffix) ->
    Nb = integer_to_binary(N),
    case N > 0 of
        true -> <<Prefix/binary, Nb/binary, Suffix/binary>>;
        false -> <<"">>
    end.

uptime() ->
    {UpTime, _ } = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    Days = iff0(D, <<" days, ">>),
    Hours = iff0(H, <<" hours, ">>),
    Minutes = iff0(M, <<" minutes">>),
    Seconds = iff0(S, <<" and ">>, <<" seconds">>),
    <<Days/binary, Hours/binary, Minutes/binary, Seconds/binary>>.

memory() ->
    M = erlang:memory(total) / 1000 / 1000, % in MB
    Mb = float_to_binary(M, [{decimals, 2}]),
    <<"memory: ", Mb/binary, " MB">>.

response() ->
    lists:join(" | ", [
        uptime(),
        memory(),
        string:chomp(erlang:system_info(system_version))
    ]).

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!uptime">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, response()),
            {ok, State};
        _ ->
            {ok, State}
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
