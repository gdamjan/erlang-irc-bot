-module(utils).
-author('gdamjan@gmail.com').

-export([backoff/1]).

backoff(N) when N > 5 ->
  backoff(5);

backoff(N) ->
  {N * N * 5000, N + 1}.

