-module(ircbot_sup).
-author("gdamjan@gmail.com").

-behaviour(supervisor).
-export([init/1]).

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

%% supervisor behaviour
init([]) ->
    {ok, {
        {simple_one_for_one, 10, 60},
        [?CHILD(ircbot_statem)]
    }}.
