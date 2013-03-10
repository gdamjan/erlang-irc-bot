-module(ircbot_plugin_kluc).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


init(_Args) ->
    {ok, sets:new()}.


handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клучеви">>]} ->
            Text = sets:fold(fun (E, AccIn) -> <<AccIn/binary, " ", E/binary>> end, <<"Клучеви имаат:">>, State),
            Ref:privmsg(Channel, Text),
            {ok, State};
        {in, _Ref, [_Sender, _Name, <<"PRIVMSG">>, _Channel, <<"!клуч +", Rest/binary>>]} ->
            {ok, sets:add_element(Rest, State)};
        {in, _Ref, [_Sender, _Name, <<"PRIVMSG">>, _Channel, <<"!клуч -", Rest/binary>>]} ->
            {ok, sets:del_element(Rest, State)};

        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
