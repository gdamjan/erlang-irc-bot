-module(plugins.tell).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-import(dict).
-import(lists).
-import(string).

init(_Args) ->
    {ok, dict:new()}.


fancy_time({Mega,Sec,_Micro}) ->
    {NowMega,NowSec,_NowMicro} = erlang:now(),
    DeltaMinutes = (NowMega*1000000 - Mega*1000000 + NowSec - Sec) div 60,
    if
        DeltaMinutes < 2 ->
            "just a minute ago";
        DeltaMinutes < 80 ->
            integer_to_list(DeltaMinutes + 1) ++ " minutes ago";
        DeltaMinutes < 36 * 60 ->
            integer_to_list(DeltaMinutes div 60) ++ " hours ago";
        true ->
            integer_to_list(DeltaMinutes div (60 * 24)) ++ " days ago"
    end.

remember(Ref, Channel, Sender, Msg, State) ->
    Timestamp = erlang:now(),
    [Recepient | Message] = re:split(Msg, "[^a-zA-Z0-9^|_{}[\\]\\\\`-]", [{parts,2}]),
    Ref:notice(Sender, ["ok, I'll  pass that to ", Recepient, " when he's around."]),
    Key = string:to_lower(binary_to_list(Recepient)), 
    {ok, dict:append(Key, {Timestamp, Channel, Sender, Message}, State)}.

reminder(Ref, Nick, State) ->
    Key = string:to_lower(binary_to_list(Nick)),
    case dict:is_key(Key, State) of
        true ->
            L = dict:fetch(Key, State),
            lists:foreach(fun ({Timestamp, Channel, From, Message}) ->
                Msg =  [fancy_time(Timestamp), " ", From, " on ", Channel, ": ", Message],
                Ref:privmsg(Nick, Msg) end, L),
            {ok, dict:erase(Key, State)};
        false ->
            {ok, State}
    end.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Sender, _Name, <<"JOIN">>, <<"#",_Channel/binary>>]} ->
            reminder(Ref, Sender, State);
        {in, Ref, [_Sender, _Name, <<"NICK">>, Nick]} ->
            reminder(Ref, Nick, State);

        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Rest/binary>>]} ->
            remember(Ref, Channel, Sender, Rest, State);
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ask ",Rest/binary>>]} ->
            remember(Ref, Channel, Sender, Rest, State);

        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",_Channel/binary>>, _Something]} ->
            reminder(Ref, Sender, State);
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
