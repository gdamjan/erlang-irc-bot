-module(utils).
-author('gdamjan@gmail.com').

-export([backoff/1, debug/1, irc_parse/1]).

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

% Erlang IRC message parsing made for parsing binaries
% http://www.irchelp.org/irchelp/rfc/rfc2812.txt

% if a string (a list of chars) is supplied, convert to a binary
% this is only usefull while testing, real IRC data will always be binary
% NOTE: optionally in R13 you could use unicode:characters_to_binary
irc_parse(Line) when is_list(Line) ->
    irc_parse(list_to_binary(Line));


% Line begins with a ":", first parse the prefix
% everything else is the Command
irc_parse(<<":", Line/binary>>) ->
    [Prefix | Rest] = re:split(Line, " ", [{parts,2}]),
    [Nick | User] = re:split(Prefix, "[!@]", [{parts,2}]),
    parse_command(Rest, [Nick, User]);

% there's no prefix (no servername, Nick or User)
% go to parsing the Command
irc_parse(Line) ->
    parse_command(Line, [<<>>,<<>>]).

% first checks for " :", everything after that will be the Trailing
% if there's no Trailing, just split 16 parts (including the Command)
% if there's is Trailing, split 15 parts
parse_command(Line, Acc) ->
    [Front | Trailing] = re:split(Line, " :", [{parts, 2}]),
    Parts = if length(Trailing) == 0 -> 16; true -> 15 end,
    [Command | Params] = re:split(Front, " ", [{parts, Parts}]),
    {match, Acc ++ [Command] ++ Params ++ Trailing}.
