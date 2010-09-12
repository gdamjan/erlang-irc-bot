-module(plugins.rps).
-author("gdamjan@gmail.com").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-import(lists).
-import(random).

init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    case Msg of
       {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!rock">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [Nick, play(rock)]),
            {ok, State};
       {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!paper">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [Nick, play(paper)]),
            {ok, State};
       {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!scissors">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [Nick, play(scissors)]),
            {ok, State};
       {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!spock">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [Nick, play(spock)]),
            {ok, State};
       {in, Ref, [Nick, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, <<"!lizard">>]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [Nick, play(lizard)]),
            {ok, State};
        _ ->
            {ok, State}
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.


%% Play a game of rock-paper-scissors
play(PlayerAttack) ->
    ComputerAttack = get_computer_attack(),
    case get_result(PlayerAttack, ComputerAttack) of
        win ->
            [": I chose ", atom_to_list(ComputerAttack), ". You win!"];
        draw ->
            [": I chose ", atom_to_list(ComputerAttack), ". It's a draw."];
        lose ->
            [": I chose ", atom_to_list(ComputerAttack), ". I WIN!"]
    end.

%% choose a computer attack at random
get_computer_attack() ->
    %% Get an index position at random
    Index = random:uniform(5),
    %% Pull out an attack
    lists:nth(Index, [rock, paper, scissors, spock, lizard]).

%% Determine the result of an attack
get_result(Player1, Player2) ->
    case {Player1, Player2} of
        {rock, scissors} -> win;
        {rock, lizard} -> win;
        {paper, rock} -> win;
        {paper, spock} -> win;
        {scissors, paper} -> win;
        {scissors, lizard} -> win;
        {spock, rock} -> win;
        {spock, scissors} -> win;
        {lizard, spock} -> win;
        {lizard, paper} -> win;
        {Same, Same} -> draw;
        {_,_} -> lose
    end.
