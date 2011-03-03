-define(VERSION, "http://github.com/gdamjan/erlang-irc-bot").
-define(REALNAME, "An experimental Erlang IRC bot").
-define(QUITMSG, "I can feel it, my mind is going...").

-define(SEND_TIMEOUT, 10000).    % 10sec
-define(RECV_TIMEOUT, 180000).   % 180sec

-define(CONNECT_TIMEOUT, 5000).    % wait for dns, but no more. 10s
-define(REGISTER_TIMEOUT, 30000).  % 30s
-define(RECONNECT_DELAY, 15000).    % fast reconnect 15s
-define(BACKOFF_DELAY, 15000).      % backoff reconnect 15s base,
                                   % delay of 0, 5, 20, 45, 80, 125
