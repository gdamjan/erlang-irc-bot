-module(ircbot_app).
-author('gdamjan@gmail.com').

-behaviour(application).
-export([start/2, stop/1]).

-define(SUPERVISOR, ircbot_sup).

%% app behaviour
start(_Type, _StartArgs) ->
    {ok, Sup} = supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []),
    {ok, SettingsFile} = init:get_argument(conf),
    {ok, Settings} = file:consult(SettingsFile),
    start_all(Sup, Settings),
    {ok, Sup}.

stop(_State) ->
    exit(whereis(?SUPERVISOR), shutdown).

get_connections_args(Settings) ->
    lists:filtermap(
        fun(El) ->
            case El of
                {connection, Args} -> {true, Args};
                _ -> false
            end
    end, Settings).

start_all(Supervisor, Settings) ->
    lists:foreach(
        fun (Args) ->
            {ok, _Child} = supervisor:start_child(Supervisor, [Args])
        end,
        get_connections_args(Settings)
    ).
