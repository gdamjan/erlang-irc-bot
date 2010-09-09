-define(VERSION, "http://github.com/gdamjan/erlang-irc-bot").
-define(REALNAME, "An experimental Erlang IRC bot").
-define(QUITMSG, "I can feel it, my mind is going...").
-define(SEND_TIMEOUT, 10000). % 10sec
-define(RECV_TIMEOUT, 180000). % 180sec
-define(CONNECT_TIMEOUT, 60000). % 60sec
-define(KEEPALIVE, 60000). % 1min


%%% a macro that makes a function that converts from a property list to an
%%% Erlang record (a taged tuple).
%%% The function has the signature proplist_to_record(Record, Proplist)
-define(make_proplist_to_record(Record),
   proplist_to_record(Record, Proplist) ->
       Fields = record_info(fields, Record),
       [Tag| Values] = tuple_to_list(#Record{}),
       Defaults = lists:zip([Tag|Fields], [Tag|Values]),
       L = lists:map(fun ({K,V}) -> proplists:get_value(K, Proplist, V) end, Defaults),
       list_to_tuple(L)
).
