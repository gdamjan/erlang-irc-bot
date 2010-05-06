-module(plugins.couch_log).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-include("couchbeam.hrl").
-import(couchbeam).
-import(couchbeam_db).
-import(couchbeam_server).

%% This plugin requires couchbeam (http://benoitc.github.com/couchbeam/)
%% Better docs to come later...

init(_Args) ->
    couchbeam:start(),
    %% FIXME: pick this up from the settings:
    %% Params = #couchdb_params{host=, port=, prefix=, username= ,password=},
    Connection = couchbeam_server:start_connection_link(),
    Db = couchbeam_server:open_db(Connection, "irclog"),
    {ok, Db}.


%% Log only messages to channel to a CouchDB database
%% Logs the sender, the channel, the message and the timestamp
handle_event(Msg, Db) ->
    case Msg of
        {_Ref, {match, [Sender, _Name, <<"PRIVMSG">>, <<"#", Channel/binary>>, Text]}} ->
            {MegaSecs, Secs, MicroSecs} = now(),
            Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
            couchbeam_db:save_doc(Db,  {[
                    {<<"sender">>, Sender},
                    {<<"channel">>, Channel},
                    {<<"message">>, Text},
                    {<<"timestamp">>,  Timestamp}
            ]}),
            {ok, Db};
        _ ->
            {ok, Db}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
