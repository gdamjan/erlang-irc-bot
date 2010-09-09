-module(plugins.couch_log).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-include("ircbot.hrl").
-include("couchbeam.hrl").

%% This plugin requires couchbeam (http://benoitc.github.com/couchbeam/)
-import(couchbeam).
-import(couchbeam_db).
-import(couchbeam_server).
-import(proplists).
-import(lists).

%% Configuration (settings.cfg):
%% {plugins, [
%%     ...
%%    {'plugins.couch_log', [{username,"xxx"}, {password,"yyy"}, {db,"zzz"}]}
%% ]}.
%%
%% You can also put any parameters that couchdb_params expects


% see ircbot.hrl about this macro, requires modules proplists and lists
?make_proplist_to_record(couchdb_params).

init(Args) ->
    DbName = proplists:get_value(db, Args),
    Params = proplist_to_record(couchdb_params, Args),
    couchbeam:start(),
    Connection = couchbeam_server:start_connection_link(Params),
    Db = couchbeam_server:open_db(Connection, DbName),
    {ok, Db}.


%% Log only messages to channel to a CouchDB database
%% Logs the sender, the channel, the message and the timestamp
log(Db, Sender, Channel, Message) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000 + Secs + MicroSecs/1000000,
    couchbeam_db:save_doc(Db,  {[
         {<<"sender">>, Sender},
         {<<"channel">>, Channel},
         {<<"message">>, Message},
         {<<"timestamp">>,  Timestamp}
    ]}).

handle_event(Msg, Db) ->
    case Msg of
        {in, _Ref, [Sender, _Name, <<"PRIVMSG">>, <<"#", Channel/binary>>, Text]} ->
            log(Db, Sender, Channel, Text),
            {ok, Db};
        {out, _Ref, [<<"PRIVMSG">>, <<"#", Channel/binary>>, Text]} ->
            log(Db, "**", Channel, Text),
            {ok, Db};
        _ ->
            {ok, Db}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
