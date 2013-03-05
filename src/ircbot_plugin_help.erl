-module(ircbot_plugin_help).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(re).
-define(HELP, "help is at http://github.com/gdamjan/erlang-irc-bot/wiki/HelpOnUsage").


init(_Args) ->
    {ok, []}.

handle_event(Ev, State) ->
    case Ev of
        {in, Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, Msg]} ->
            case re:run(Msg, "!help", [{capture, none}]) of
                match ->
                    Ref:privmsg(<<"#",Channel/binary>>, [Sender, ": ", ?HELP]);
                _ -> ok
            end;
        _ -> ok
    end,
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
