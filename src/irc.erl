% Erlang IRC message parsing made for parsing binaries
% http://www.irchelp.org/irchelp/rfc/rfc2812.txt
-module(irc).
-author('gdamjan@gmail.com').

-export([parse/1]).


% if a string (a list of chars) is supplied, convert to a binary
% this is only usefull while testing, real IRC data will always be binary
% NOTE: optionally in R13 you could use unicode:characters_to_binary
parse(Line) when is_list(Line) ->
    parse(list_to_binary(Line));


% Line begins with a ":", first parse the prefix
% everything else is the Command
parse(<<":", Line/binary>>) ->
    [Prefix | Rest] = re:split(Line, " ", [{parts,2}]),
    [Nick | User] = re:split(Prefix, "[!@]", [{parts,2}]),
    parse_command(Rest, [Nick, User]);

% there's no prefix (no servername, Nick or User)
% go to parsing the Command
parse(Line) ->
    parse_command(Line, [<<>>,<<>>]).

% first checks for " :", everything after that will be the Trailing
% if there's no Trailing, just split 16 parts (including the Command)
% if there's is Trailing, split 15 parts
parse_command(Line, Acc) ->
    [Front | Trailing] = re:split(Line, " :", [{parts, 2}]),
    Parts = if length(Trailing) == 0 -> 16; true -> 15 end,
    [Command | Params] = re:split(Front, " ", [{parts, Parts}]),
    {match, Acc ++ [Command] ++ Params ++ Trailing}.
