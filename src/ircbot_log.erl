-module(ircbot_log).
-author("gdamjan@gmail.com").

-export([init/0, init/1, debug/1, debug/2]).

%%
%% Stupid and simple module that logs to stdout. The stdout port thing is needed so we can write Erlang binaries,
%% no mather if they are utf, latin1 or something else. Seems no other function could do that in Erlang :(
%%

init() ->
    init([]).

init(_) ->
    open_stdout().

%% debug helpers
debug(in, Msg) ->
    debug([" IN| ", Msg]);

debug(out, Msg) ->
    debug(["OUT| ", Msg]).

% print directly to stdout thus avoid Erlangs broken
% io:* routines
debug(Msg) ->
    port_command(stdout, [Msg, "\r\n"]).

% open stdout as an Erlang port and register it with the
% stdout atom. The port will be closed automatically if the
% connection process dies.
open_stdout() ->
    StdOut = open_port("/dev/stdout", [binary, out]),
    register(stdout, StdOut),
    debug(["stdout ready for logging"]).
