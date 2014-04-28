-module(ircbot_app).
-author('gdamjan@gmail.com').

-behaviour(application).
-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

%% API
-export([start/0]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

start() ->
    application:start(sasl),
    application:start(lager),
    application:start(ircbot).


%% app behaviour
start(_Type, _StartArgs) ->
    {ok, Sup} = supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []),
    {ok, SettingsFile} = init:get_argument(conf),
    {ok, Settings} = file:consult(SettingsFile),
    connect_all(Sup, Settings),
    {ok, Sup}.

stop(_State) ->
    exit(whereis(?SUPERVISOR), shutdown).


%% supervisor behaviour
init([]) ->
    {ok, {{simple_one_for_one, 10, 60}, [?CHILD(ircbot_fsm)]}}.


%% helper
connect_all(Sup, Settings) ->
    lists:foreach(
        fun ({connection, Args}) ->
                {ok, Child} = supervisor:start_child(Sup, [Args]),
                gen_fsm:send_event(Child, connect)
        end,
        Settings
    ).
