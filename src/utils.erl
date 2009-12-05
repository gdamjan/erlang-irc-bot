-module(utils).
-author('gdamjan@gmail.com').

-export([backoff/1, debug/1]).

backoff(N) when N > 5 ->
  backoff(5);

backoff(N) ->
  {N * N * 5000, N + 1}.

debug(Msg) ->
    case catch io:format("~ts~n", [Msg]) of
        {'EXIT', _} ->
            catch io:format("~s~n", [Msg]),
            ok;
        ok ->
            ok
    end.

