-module(channels_plugin).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


init(Channels) ->
    L = lists:map(fun(X) -> list_to_binary(X) end, Channels),
    State = sets:from_list(L),
    {ok, State}.

handle_event(Msg, Channels) ->
    case Msg of
        {in, Ref, [_, _, <<"001">>, _Nick, _]} ->
        %% join the channels on connect
            lists:foreach(
                fun (Ch) -> Ref:join(Ch) end,
                sets:to_list(Channels)
            ),
            {ok, Channels};
        {in, _Ref, [_Server, _, <<"JOIN">>, Channel]} ->
        %% keep track of channels
            {ok, sets:add_element(Channel, Channels)};
        {in, _Ref, [_Server, _, <<"PART">>, Channel]} ->
        %% keep track of channels
            {ok, sets:del_element(Channel, Channels)};
        {in, _Ref, [_Server, _, <<"KICK">>, Channel|_]} ->
        %% keep track of channels
            {ok, sets:del_element(Channel, Channels)};
        _ ->
            {ok, Channels}
    end.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
