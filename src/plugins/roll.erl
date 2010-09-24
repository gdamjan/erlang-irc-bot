-module(plugins.roll).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(random).
-import(io_lib).
-import(re).


init(_Args) ->
    random:seed(now()),
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _User, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!roll",Rest/binary>>]} ->
            case re:run(Rest, "[0-9]+", [{capture, all, binary}, global]) of
                {match, [[Start],[End]|_Tail]} ->
                    S = list_to_integer(binary_to_list(Start)),
                    E = list_to_integer(binary_to_list(End)),
                    N = abs(S - E),
                    R = random:uniform(N) + erlang:min(S, E);
                {match, [[End]]} ->
                    N = list_to_integer(binary_to_list(End)),
                    R = random:uniform(N);
                _ ->
                    R = random:uniform()
            end,
            Ref:privmsg(<<"#",Channel/binary>>, io_lib:format("I'm rolling a ~p", [R]));
        _ ->
            ok
    end,
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
