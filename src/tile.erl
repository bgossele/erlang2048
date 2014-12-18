% Author: Brecht Gosselé, r0259849

-module(tile).

-export([tilelife/3]).

tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			%debug:debug("I, ~p, die.~n",[Id]),
			NextValue = CurrentValue,
			NextMerged = Merged,
			exit({killed, Id, CurrentValue, Merged});
		up ->
			direction_routine(Id, up, CurrentValue, Merged),
			NextValue = CurrentValue,
			NextMerged = Merged;
		dn ->
			direction_routine(Id, dn, CurrentValue, Merged),
			NextValue = CurrentValue,
			NextMerged = Merged;
		lx ->
			direction_routine(Id, lx, CurrentValue, Merged),
			NextValue = CurrentValue,
			NextMerged = Merged;
		rx ->
			direction_routine(Id, rx, CurrentValue, Merged),
			NextValue = CurrentValue,
			NextMerged = Merged;
		{yourValue, Repl} ->
			NextValue = CurrentValue,
			NextMerged = Merged,
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			NextValue = Future,
			NextMerged = NewMerged;
		{neighbouranswers, Answers, Dir} ->
			{MatchId, MatchValue} = find_match(Answers, CurrentValue, 0, 0),
			case MatchId of 
				0 ->
					NextValue = CurrentValue,
					NextMerged = Merged;
				_ ->                          
					glob:sendToTile(MatchId, {setvalue, CurrentValue + MatchValue, MatchValue > 0}),
					NextValue = 0,
					NextMerged = Merged
			end,
			propagate(Dir, Id);
		sync -> %Round is over, reset Merged value
			NextValue  = CurrentValue,
			NextMerged = false
	end,
	tilelife(Id, NextValue, NextMerged).


direction_routine(Id, Dir, CurrentValue, Merged) ->
 	case (Merged) orelse (CurrentValue == 0) of 
		true -> %If already merged or empty, do nothing.
			propagate(Dir, Id);
		false -> %Check if neighbouring tiles are available and merge if possible.
			Neighbours = neighbours(Id,Dir),
			case length(Neighbours) > 0 of
				true ->
					glob:registerName(collectorname(Id),spawn(fun()-> collect(length(Neighbours),0, Id, Dir, init_answers_list(Neighbours)) end)),
					lists:map(fun(X) -> glob:sendToTile(X, {yourValue, collectorname(Id)}) end, Neighbours);
				false ->
					propagate(Dir, Id)
			end
	end.

%get list of neighbours in indicated direction
neighbours(TileNo,Dir)->
	F = dir_factor(Dir),
	lists:takewhile(fun(X)->(not end_of_board(Dir,X)) end,[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17]).

%indicates whether a tile is on the edge of the board in indicated direction
end_of_board(Dir, TileNo)->
	case Dir of
		up ->
			TileNo > 12;
		dn ->
			TileNo < 5;
		lx ->
			TileNo rem 4 == 0;
		rx ->
			(TileNo - 1) rem 4 == 0
end.

%iterate over the neighbours and find the one to merge with	
find_match(Neighbours, Value, PrevMatch, PrevValue)->
	case Neighbours of
		[] -> {PrevMatch, PrevValue};
		[N] ->
			case ismatch(N,Value) of
				true -> 
					%debug:debug("Match found ~p~n", [element(1,N)]),
					{element(1,N), element(2,N)};
				false -> {PrevMatch, PrevValue}
			end;
		[N|NS] ->
			case ismatch(N, Value) of
			true ->
				find_match(NS, Value, element(1, N), element(2,N));
			false ->
				{PrevMatch, PrevValue}
			end
	end.

ismatch({_, Candidate_value, Merged},Value) ->
	(Candidate_value == 0) orelse ((Candidate_value == Value) andalso (not Merged)). 

collect(N_expected, N, Id, Dir_to_propagate, Answers) ->
	case N of
		N_expected ->
			glob:sendToTile(Id, {neighbouranswers, Answers, Dir_to_propagate});
		_ ->
			receive
				{tilevalue, SenderId, CurrentValue, Merged} ->					 
					collect(N_expected, N+1, Id, Dir_to_propagate, lists:map(fun(X) ->  compare_answer(X, {SenderId, CurrentValue, Merged}) end, Answers))
			end
	end.

%update tuple to correct values if Id matches.
compare_answer({Id, V, M},{Id2, CurrentValue, Merged}) ->
	case Id == Id2 of
		true ->
			{Id, CurrentValue, Merged};
		false ->
			{Id, V, M}
	end.

init_answers_list(L) ->
	lists:map(fun(X) -> {X, 0, false} end, L).
	
collectorname(Id) ->
	list_to_atom(string:concat("collector", integer_to_list(Id))).
	
propagate(Dir, TileNo)->
	manager ! tileReady, %indicate computation is over
	case end_of_board(Dir, TileNo) of %send command to next tile (if there is one).
		false ->
			glob:sendToTile(TileNo - dir_factor(Dir), Dir);
		true ->
			ok
	end.
	
dir_factor(Dir)->
	case Dir of
		up -> -4;
		dn -> 4;
		lx -> -1;
		rx -> 1
	end.

