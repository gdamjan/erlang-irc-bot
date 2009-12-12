-module(plugins.tell).
-behaviour(gen_event).

-author("gdamjan@gmail.com").
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-import(dict).

init(_Args) ->
    {ok, dict:new()}.

handle_event(Msg, State) ->
    case Msg of
        {Pid, {match, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!tell ",Tale/binary>>]}} ->
            [Whom | Message] = re:split(Tale, " ", [{parts,2}]),
            Pid ! {send_data, ["PRIVMSG ", <<"#",Channel/binary>>, " :ok"]},
            {ok, dict:store({Whom, Channel}, {Nick, Message}, State)};
        {Pid, {match, [Nick, _Name, <<"JOIN">>, <<"#",Channel/binary>>]}} ->
            Key = {Nick, Channel},
            case dict:is_key(Key, State) of
                true ->
                    {Whom, Message} = dict:fetch(Key, State),
                    Pid ! {send_data, ["PRIVMSG ", <<"#",Channel/binary>>, " :", 
                        "Hey ", Nick, ", ", Whom, " told me to tell you: ", Message]},
                    {ok, dict:erase(Key, State)};
                false ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
