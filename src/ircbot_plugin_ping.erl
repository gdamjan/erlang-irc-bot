-module(ircbot_plugin_ping).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-define(SECRET_KEY, "abcd").


init(_Args) ->
    crypto:start(),
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _User, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!ping ", Who/binary>>]} ->
            {_, Secs, _} = os:timestamp(),
            Mssg = encode(Channel, Secs),
            Ref:privmsg(Who, <<"\^APING ", Mssg/binary, "\^A">>);

        {in, Ref, [Sender, _User, <<"NOTICE">>, _Nick, <<"\^APING ", Rest/binary>>]} ->
            {_, Secs, _} = os:timestamp(),
            Mssg = strip_last_byte(Rest),
            {Channel, Secs_prev} = decode(Mssg),
            Lag = list_to_binary(integer_to_list(Secs - Secs_prev)),
            Ref:privmsg(<<"#",Channel/binary>>, <<Sender/binary, " is lagging ", Lag/binary, " oranges">>);
        _ ->
            ok
    end,
    {ok, State}.


decode(Bin) ->
    Bin1 = base64:decode(Bin),
    <<Hmac:12/binary, Secs:32/integer, Channel/binary>> = Bin1,
    Msg = <<Secs:32/integer, Channel/binary>>,
    Hmac = crypto:hmac(sha, ?SECRET_KEY, Msg),
    {Channel, Secs}.


encode(Channel, Secs) ->
    Msg = <<Secs:32/integer, Channel/binary>>,
    Hmac = crypto:hmac(sha, ?SECRET_KEY, Msg),
    base64:encode(<<Hmac/binary, Msg/binary>>).


strip_last_byte(Bin) ->
    N = byte_size(Bin) - 1,
    <<X:N/binary, _Rest/binary>> = Bin,
    X.


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
