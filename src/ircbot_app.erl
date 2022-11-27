-module(ircbot_app).
-author("gdamjan@gmail.com").

-behaviour(application).
-export([start/2, stop/1]).

-define(SUPERVISOR, ircbot_sup).

%% application behaviour callbacks: start/2 and stop/1
start(_Type, _StartArgs) ->
    case get_settings() of
        {ok, Settings} ->
            {ok, Sup} = supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []),
            start_all_connections(Sup, Settings),
            {ok, Sup};
        %% graceful shutdown, but return an Exit Code to the OS,
        %% returns a {ok, pid} to make the application start happy
        {error, ExitCode} ->
            {ok, spawn(fun() -> init:stop(ExitCode) end)}
    end.

stop(_State) ->
    exit(whereis(?SUPERVISOR), shutdown).


%% read settings either from the config file specified on the command line (-ircbot config_file '"/etc/…"')
%% or in the OS environment CONFIG_FILE
get_settings() ->
    CliArg = application:get_env(ircbot, config_file),
    EnvArg = os:getenv("CONFIG_FILE"),
    case { CliArg, EnvArg } of
        { undefined, false } ->
            logger:critical("Config file not specified"),
            {error, 1};
        { undefined, ConfigFile } ->
            read_settings(ConfigFile);
        { {ok, ConfigFile}, _ } ->
            read_settings(ConfigFile)
    end.


read_settings(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Settings} ->
            {ok, Settings};
        {error, enoent} ->
            logger:critical("Config file does not exist", [ConfigFile]),
            {error, 1};
        {error, eacces} ->
            logger:critical("Permission denied reading config file", [ConfigFile]),
            {error, 1};
        {error, {CharNumber, erl_scan, Err}} ->
            logger:critical("Error reading config file ~p: at char ~d, ~p", [ConfigFile, CharNumber, Err]),
            {error, 1};
        {error, badarg} ->
            logger:critical("'badarg' from `file:consult(ConfigFile)`: ~p", [ConfigFile]),
            {error, 1};
        OtherError ->
            logger:critical("~p: ~p", [ConfigFile, OtherError]),
            {error, 1}
    end.


get_connections_args(Settings) ->
    lists:filtermap(
        fun(El) ->
            case El of
                {connection, Args} -> {true, Args};
                _ -> false
            end
    end, Settings).

start_all_connections(Supervisor, Settings) ->
    lists:foreach(
        fun (Args) ->
            {ok, _Child} = supervisor:start_child(Supervisor, [Args])
        end,
        get_connections_args(Settings)
    ).
