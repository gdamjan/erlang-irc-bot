-module(plugins.google).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(ircbot_lib).

-import(lists).
-import(proplists).
-import(http).
-import(inets).
% unicode is only available from Erlang R13
-import(unicode).

init(_Args) ->
    inets:start(),
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, Channel, <<"!g ", Query/binary>>]} ->
            fetch(Query, Ref, Channel),
            {ok, State};
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.


fetch(Query, Ref, Channel) ->
    Callback = fun(Url) -> Ref:privmsg(Channel, Url) end,
    spawn(fun() -> gfl(Query, Callback) end).

gfl(Query, Callback) ->
    Headers = [{"User-Agent", "Mozilla/5.0 (erlang-irc-bot)"}],
    Q = escape_uri(Query),
    Url = "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=" ++ Q,
    case http:request(get, {Url, Headers}, [{autoredirect, false}], []) of
        {ok, {{_,302,_}, ResponseHeaders, _}} ->
            Callback(proplists:get_value("location", ResponseHeaders));
        {ok, {{_,200,_}, _, _}} ->
            Callback(["No match, see: ", Url]);
        _ ->
            Callback("search error")
    end.

%% ---------------------------------------------------------------------
%% URI and Internet

%% This is a conservative URI escaping, which escapes anything that may
%% not appear in an NMTOKEN ([a-zA-Z0-9]|'.'|'-'|'_'), including ':'.
%% Characters are first encoded in UTF-8.
%%
%% Note that this should *not* be applied to complete URI, but only to
%% segments that may need escaping, when forming a complete URI.
%%
%% it takes a UTF-8 binary or a string/list as input and returns a string/list.

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C | escape_uri(Cs)];
escape_uri(<<C:8, Cs/binary>>) ->
    [ escape_byte(C) | escape_uri(Cs)];
escape_uri(<<>>) ->
    [].

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].
