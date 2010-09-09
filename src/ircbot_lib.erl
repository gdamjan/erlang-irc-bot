-module(ircbot_lib).
-author("gdamjan@gmail.com").

-export([debug/1, irc_parse/1, url_match/1, url_match/2, url_quote/1]).

debug(Msg) ->
    case catch io:format("~ts~n", [Msg]) of
        {'EXIT', _} ->
            catch io:format("~s~n", [Msg]),
            ok;
        ok ->
            ok
    end.

%% Based on http://regexlib.com/RETester.aspx?regexp_id=1057
url_match(Line, Suffix) ->
    Re = "(((http|https)://)|(www\\.))?"
         "(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|"
         "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))"
         "(/[a-zA-Z0-9\\&amp;%_\\./-~-]*)?" ++ Suffix,
    re:run(Line, Re, [caseless, {capture, [0], binary}]).

url_match(Line) ->
    url_match(Line, "").

%% Stolen from mochiweb
-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

url_quote(Binary) when is_binary(Binary) ->
    url_quote(binary_to_list(Binary));
url_quote(String) ->
    url_quote(String, []).

url_quote([], Acc) ->
    lists:reverse(Acc);
url_quote([C | Rest], Acc) when ?QS_SAFE(C) ->
    url_quote(Rest, [C | Acc]);
url_quote([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    url_quote(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).


%%% Erlang IRC message parsing made for parsing binaries
%%% http://www.irchelp.org/irchelp/rfc/rfc2812.txt

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
