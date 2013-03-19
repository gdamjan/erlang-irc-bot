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
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клуч +", Rest/binary>>]} ->
            Ref:notice(Channel, <<Rest/binary, " додаден">>),
            {ok, sets:add_element(Rest, State)};
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клуч -", Rest/binary>>]} ->
            Ref:notice(Channel, <<Rest/binary, " отстранет">>),
            {ok, sets:del_element(Rest, State)};
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, Channel, <<"!клучеви ", Rest/binary>>]} ->
            {NewSet, Response} = process_changes(Rest, State),
            Ref:notice(Channel, Response),
            {ok, NewSet};
        _ ->
            {ok, State}
    end.


process_changes(Line, Set) ->
    List = binary:split(Line, <<" ">>, [global, trim]),
    process_list(List, Set, []).

process_list([], Set, Text) ->
    {Set, Text};

process_list([Head|Tail], Set, Text) ->
    case Head of
        <<"+", Rest/binary>> ->
            NewSet = sets:add_element(Rest, Set),
            NewText = <<Rest/binary, " додаден ">>,
            process_list(Tail, NewSet, [Text|NewText]);
        <<"-", Rest/binary>> ->
            NewSet = sets:del_element(Rest, Set),
            NewText = <<Rest/binary, " отстранет ">>,
            process_list(Tail, NewSet, [Text|NewText]);
        _ ->
            process_list(Tail, Set, Text)
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
