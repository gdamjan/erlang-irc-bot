-module(plugins.tell).
-behaviour(gen_event).

-author("gdamjan@gmail.com").
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-import(dict).
-import(lists).

init(_Args) ->
    {ok, dict:new()}.


fancy_time(T) ->
    "{time}".

remember(Pid, Channel, From, Msg, State) ->
    Timestamp = 1,
    [Recepient | Message] = re:split(Msg, " ", [{parts,2}]),
    Pid ! {send_data, ["NOTICE ", From, " :ok, I'll  pass that to ", Recepient]},
    {ok, dict:append(Recepient, {Timestamp, Channel, From, Message}, State)}.

reminder(Pid, Nick, State) ->
    case dict:is_key(Nick, State) of
        true ->
            L = dict:fetch(Nick, State),
            lists:foreach(fun ({Timestamp, Channel, From, Message}) ->
    		Msg =  [fancy_time(Timestamp), " ", From, " on ", Channel, ": ", Message],
    		Pid ! {send_data, ["NOTICE ", Nick, " :", Msg]} end, L),
            {ok, dict:erase(Nick, State)};
        false ->
            {ok, State}
    end.

handle_event(Msg, State) ->
    case Msg of
        {Pid, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Rest/binary>>]}} ->
            remember(Pid, Channel, Sender, Rest, State);
        {Pid, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ask ",Rest/binary>>]}} ->
            remember(Pid, Channel, Sender, Rest, State);

        {Pid, {match, [Sender, _Name, <<"JOIN">>, <<"#",_Channel/binary>>]}} ->
            reminder(Pid, Sender, State);
        {Pid, {match, [_Sender, _Name, <<"NICK">>, Nick]}} ->
            reminder(Pid, Nick, State);
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
