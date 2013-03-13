-module(ircbot_plugin_nickserv).
-behaviour(gen_event).

-author("gdamjan@gmail.com").

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-define(TRIGGER, <<"This nickname is registered. ",
    "Please choose a different nickname, or identify ",
    "via ",2,"/msg NickServ identify <password>",2,".">>).

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
