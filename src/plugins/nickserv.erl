-module(plugins.nickserv).
-behaviour(gen_event).

-author("gdamjan@gmail.com").

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-define(TRIGGER, <<84,104,105,115,32,110,105,99,107,110,97,109,101,32,105,115,32,114,
             101,103,105,115,116,101,114,101,100,46,32,80,108,101,97,115,101,
             32,99,104,111,111,115,101,32,97,32,100,105,102,102,101,114,101,
             110,116,32,110,105,99,107,110,97,109,101,44,32,111,114,32,105,100,
             101,110,116,105,102,121,32,118,105,97,32,2,47,109,115,103,32,78,
             105,99,107,83,101,114,118,32,105,100,101,110,116,105,102,121,32,
             60,112,97,115,115,119,111,114,100,62,2,46>>).


%% setup the password in your settings file
%% {plugins, [
%%    ...
%%    {'plugins.nickserv', ["SECRET"]}
%% ]}.

init(Password) ->
    {ok, Password}.

handle_event(Msg, Password) ->
    case Msg of
        {in, Ref, [<<"NickServ">>, _User, <<"NOTICE">>, _Nick, ?TRIGGER]} ->
            Ref:privmsg("NickServ", ["identify ", Password]);
        _ ->
            ok
    end,
    {ok, Password}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
