%%% dummy common test suite, to satisfy the CI
%%% https://erlang.org/doc/apps/common_test/basics_chapter.html

-module(dummy_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test/1]).
all() -> [test].

test(_Config) ->
  1 = 1.
