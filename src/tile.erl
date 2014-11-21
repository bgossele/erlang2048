-module(tile).

-export([tilemain/1]).

tilemain( Id ) ->
	tilemain(Id, 0).

tilemain( Id, Value ) ->
	tilelife(Id, Value, false).


%%%%%%%%%%%%%%%%%
% fill this out %
%%%%%%%%%%%%%%%%%
tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			debug:debug("I, ~p, die.~n",[Id]),
			NextValue = CurrentValue,
			NextMerged = Merged,
			exit(killed);
		up ->
			
			NextValue = CurrentValue,
			NextMerged = Merged;
		dn ->
			%Neighbours = neighbours(Id,4),
			%Neighbour_values = lists:map(fun askNeighbour/1, Neighbours),
			NextValue = CurrentValue,
			NextMerged = Merged,
			propagate(dn, Id);
		lx ->
			%Neighbours = neighbours(Id,-1),
			NextValue = CurrentValue,
			NextMerged = Merged,
			propagate(lx, Id);
		rx ->
			%Neighbours = neighbours(Id,1),
			NextValue = CurrentValue,
			NextMerged = Merged,
			propagate(rx, Id);
		{yourValue, Repl} ->
			%debug:debug("~p received yourValue request from ~p~n", [Id, Repl]),
			NextValue = CurrentValue,
			case Repl of
				collector -> NextMerged = false;
				_ -> NextMerged = Merged
			end,
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			debug:debug("setvalue at ~p, ~p, ~p~n", [Id, Future, NewMerged]),
			NextValue = Future,
			NextMerged = NewMerged;
		{neighbouranswers, Answers, Dir} ->
			{MatchId, MatchValue} = find_match(Answers, CurrentValue, 0, 0),
			%debug:debug("match = ~p; ~p~n", [MatchId, MatchValue]),
			case MatchId of 
				0 ->
					NextValue = CurrentValue,
					NextMerged = Merged;
				_ ->
					glob:regformat(MatchId) ! {setvalue, CurrentValue + MatchValue, true},
					NextValue = 0,
					NextMerged = Merged
			end,
			propagate(Dir, Id)
	end,
	tilelife(Id, NextValue, NextMerged).

direction_routine(Id, Dir, CurrentValue, Merged) ->
 	case (Merged) orelse (CurrentValue == 0) of
		true ->
			propagate(up, Id);
		false ->
			Neighbours = neighbours(Id,Dir),
			case length(Neighbours) > 0 of
				true ->
					glob:registerName(collectorname(Id),spawn(fun()-> collect(length(Neighbours),0, Id, Dir, init_answers_list(Neighbours)) end)),
					lists:map(fun(X) -> glob:regformat(X) ! {yourValue, collectorname(Id)} end, Neighbours);
				false ->
					propagate(Dir, Id)
			end
	end.

neighbours(TileNo,Dir)->
	F = dir_factor(Dir),
	[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17].
	
find_match(Neighbours, Value, PrevMatch, PrevValue)->
	%debug:debug("find_match: #Neighbours = ~p; ~p; ~p; ~p~n", [length(Neighbours), Value, PrevMatch, PrevValue]),
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
	%debug:debug("ismatch ~p; ~p; ~p~n", [Candidate_value, Merged, Value]),
	(Candidate_value == 0) orelse ((Candidate_value == Value) andalso (not Merged)). 

collect(N_expected, N, Id, Dir_to_propagate, Answers) ->
	case N of
		N_expected ->
			glob:regformat(Id) !{neighbouranswers, Answers, Dir_to_propagate};
		_ ->
			receive
				{tilevalue, SenderId, CurrentValue, Merged} ->
					%debug:debug("Collector of ~p received ~p; ~p; ~p~n",[Id, SenderId, CurrentValue, Merged]),					 
					collect(N_expected, N+1, Id, Dir_to_propagate, lists:map(fun(X) ->  compare_answer(X, {SenderId, CurrentValue, Merged}) end, Answers))
			end
	end.

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
	list_to_atom(string:concat("collector",integer_to_list(Id))).
	
propagate(Dir, TileNo)->
	case end_of_board(Dir, TileNo) of
		false ->
			glob:regformat(TileNo-dir_factor(Dir)) ! Dir;
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
	
