-module(ircbot_log).
-author("gdamjan@gmail.com").

-export([init/0, init/1, debug/1, debug/2]).

init() ->
    init([]).

init(_) ->
    ok.

%% debug helpers
debug(in, Msg) ->
    debug([" IN| ", Msg]);

debug(out, Msg) ->
    debug(["OUT| ", Msg]).

% print directly to stdout thus avoid Erlangs broken
% io:* routines
debug(Msg) ->
    io:format("~ts\n", [Msg]).
