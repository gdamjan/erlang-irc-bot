-module(plugins.tell).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-import(dict).
-import(lists).

init(_Args) ->
    {ok, dict:new()}.


fancy_time(T) ->
    "{time}".

remember(Ref, Channel, From, Msg, State) ->
    Timestamp = 1,
    [Recepient | Message] = re:split(Msg, " ", [{parts,2}]),
    Ref:send_data(["NOTICE ", From, " :ok, I'll  pass that to ", Recepient, " when he's around."]),
    {ok, dict:append(Recepient, {Timestamp, Channel, From, Message}, State)}.

reminder(Ref, Nick, State) ->
    case dict:is_key(Nick, State) of
        true ->
            L = dict:fetch(Nick, State),
            lists:foreach(fun ({Timestamp, Channel, From, Message}) ->
                Msg =  [fancy_time(Timestamp), " ", From, " on ", Channel, ": ", Message],
                Ref:send_data(["NOTICE ", Nick, " :", Msg]) end, L),
            {ok, dict:erase(Nick, State)};
        false ->
            {ok, State}
    end.

handle_event(Msg, State) ->
    case Msg of
        {Ref, {match, [Sender, _Name, <<"JOIN">>, <<"#",_Channel/binary>>]}} ->
            reminder(Ref, Sender, State);
        {Ref, {match, [_Sender, _Name, <<"NICK">>, Nick]}} ->
            reminder(Ref, Nick, State);
        {Ref, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#",_Channel/binary>>]}} ->
            reminder(Ref, Sender, State);

        {Ref, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Rest/binary>>]}} ->
            remember(Ref, Channel, Sender, Rest, State);
        {Ref, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ask ",Rest/binary>>]}} ->
            remember(Ref, Channel, Sender, Rest, State);
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
