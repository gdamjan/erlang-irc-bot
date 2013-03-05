-module(ircbot_plugins).
-author("gdamjan@gmail.com").

-export([start_link/1, add_handler/3, delete_handler/3, which_handlers/1, notify/2]).

start_link(Settings) ->
    {ok, Plugins} = gen_event:start_link(),
    Channels = proplists:get_value(channels, Settings, []),
    gen_event:add_handler(Plugins, ircbot_plugin_channels, Channels),
    gen_event:add_handler(Plugins, ircbot_plugin_pong, []),
    gen_event:add_handler(Plugins, ircbot_plugin_ctcp, []),
    lists:foreach(
        fun ({Plugin, Args}) ->
            gen_event:add_handler(Plugins, Plugin, Args)
        end,
        proplists:get_value(plugins, Settings, [])
    ),
    {ok, Plugins}.

add_handler(GenEv, Plugin, Args)->
    case gen_event:add_handler(GenEv, Plugin, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_msg("Problem loading plugin ~p ~p ~n", [Plugin, Reason]);
        Other ->
            error_logger:error_msg("Loading ~p reports ~p ~n", [Plugin, Other])
    end.

delete_handler(GenEv, Plugin, Args)->
    case gen_event:delete_handler(GenEv, Plugin, Args) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_msg("Problem deleting plugin ~p ~p ~n", [Plugin, Reason]);
        Other ->
            error_logger:error_msg("Deleting ~p reports ~p ~n", [Plugin, Other])
    end.

notify(GenEv, Msg) ->
    gen_event:notify(GenEv, Msg).

which_handlers(GenEv) ->
    gen_event:which_handlers(GenEv).
