-module(ircbot_lib).
-author("gdamjan@gmail.com").

-export([irc_parse/1, url_match/1, url_match/2, escape_uri/1]).
-export([iolist_join/1, iolist_join/2]).

%% Based on http://regexlib.com/RETester.aspx?regexp_id=1057
url_match(Line, Suffix) ->
    Re = "(((http|https)://)|(www\\.))?"
         "(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|"
         "([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))"
         "(/[a-zA-Z0-9\\&amp;%_\\./-~-]*)?" ++ "(" ++ Suffix ++ ")",
    re:run(Line, Re, [caseless, {capture, [0], binary}]).

url_match(Line) ->
    url_match(Line, "").

%% Stolen from mochiweb and stackoverflow and erlang otp sources
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

escape_byte(C) when ?QS_SAFE(C) -> <<C>>;
escape_byte(C) ->
    <<Hi:4, Lo:4>> = <<C>>,
    Hi1 = <<(hexdigit(Hi))>>,
    Lo1 = <<(hexdigit(Lo))>>,
    <<"%", Hi1/binary, Lo1/binary>>.

%% returns binary
escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(Bin) ->
    << <<(escape_byte(X))/binary>> || <<X>> <= Bin >>.


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


%
% Similar to string:join but doesn't require strings, and
% returns an iolist
%
iolist_join([B], _, Acc) ->
    lists:reverse([ B | Acc ]);

iolist_join([B|T], Sep, Acc) ->
    iolist_join(T, Sep, [ [B, Sep] | Acc ]).

iolist_join(L, Sep) ->
    iolist_join(L, Sep, []).

iolist_join(L) ->
    iolist_join(L, " ").
