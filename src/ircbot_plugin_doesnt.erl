-module(ircbot_plugin_doesnt).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).



init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Text]} ->
            case re:run(Text, "не работи", [unicode, caseless]) of
                {match, _} ->
                    Ref:privmsg(<<"#",Channel/binary>>, [Nick, ":",
                           " Look buddy, doesn't work is a strong statement.",
                           " Does it sit on the couch all day? Does it want more",
                           " money? Is it on IRC all the time? Be specific!",
                           " Examples of what doesn't work (or the URL) tend to",
                           " help too, or pastebin the config if that's the problem"]),
                    {ok, State};
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
