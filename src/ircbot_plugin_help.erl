-module(ircbot_plugin_help).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(HELP, "help is at http://github.com/gdamjan/erlang-irc-bot/wiki/HelpOnUsage").


init([Url]) ->
    {ok, Url}.

handle_event(Ev, Url) ->
    case Ev of
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Msg]} ->
            case re:run(Msg, "!help", [{capture, none}]) of
                match ->
                    ircbot_api:privmsg(<<"#",Channel/binary>>, [Sender, ": help is at ", Url], Ref);
                _ -> ok
            end;
        _ -> ok
    end,
    {ok, Url}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
