-module(plugins.viewer).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(ircbot_lib).

%% This plugin watches the channel for links to PDF, TIFFs or PPS files and if
%% it sees one it responds with a NOTICE with a http://docs.google.com/viewer
%% url.

init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Text]} ->
            case ircbot_lib:url_match(Text, "\\.doc|\\.pdf|\\.pps|\\.tiff|\\.tif") of
                {match, [Url]} ->
                    Url1 = ircbot_lib:escape_uri(Url),
                    Ref:notice(<<"#",Channel/binary>>, ["http://docs.google.com/viewer?url=", Url1]),
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
