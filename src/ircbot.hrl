-define(VERSION,  <<"http://github.com/gdamjan/erlang-irc-bot">>).
-define(REALNAME, <<"An experimental Erlang IRC bot">>).
-define(QUITMSG,  <<"I can feel it, my mind is going...">>).

-define(SECOND, 1000).
-define(MINUTE, 60 * 1000).

-define(RECV_TIMEOUT,      3 * ?MINUTE).
-define(SEND_TIMEOUT,     10 * ?SECOND).

-define(CONNECT_TIMEOUT,   5 * ?SECOND).  % wait for dns
-define(REGISTER_TIMEOUT, 30 * ?SECOND).  % wait for register on irc
-define(RECONNECT_DELAY,  15 * ?SECOND).  % fast reconnect
-define(BACKOFF_DELAY,    ?SECOND).       % backoff reconnect 1s base,
                                          % delay of 0, 1, 4, 9, …

-define(NICK_SUFFIX, <<"_">>).         % append suffix to nickname if nick is in use
