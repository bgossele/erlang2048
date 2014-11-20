-module(tile).

-export([tilemain/1, format_list/1]).

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
			case Merged of
				true ->
					propagate(up, Id);
				false ->
					Neighbours = neighbours(Id,-4),
					case length(Neighbours) > 0 of
						true ->
							glob:registerName(collectorname(Id),spawn(fun()-> collect(length(Neighbours),0, Id, init_answers_list(Neighbours), up) end)),
							lists:map(fun(X) -> glob:regformat(X) ! {yourValue, self()} end, Neighbours);
						false ->
							propagate(up, Id)
					end
			end,
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
			NextMerged = Merged,
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			io:format("setValue at ~p, ~p, ~p", [Id, Future, NewMerged]),
			NextValue = Future,
			NextMerged = NewMerged;
		{neighbouranswers, Answers, Dir} ->
			%itereren over neighbours en checken in map. Laatst gecheckte bijhouden. Iets is een match als == 0 of zelfde value en niet gemerged.
			%Als geen match, mergen met vorige neighbour.
			propagate(Dir, Id),
			NextValue = CurrentValue,
			NextMerged = Merged
	end,
	tilelife(Id, NextValue, NextMerged).
 
neighbours(TileNo,F)->
	[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17].
	
collect(N_expected, N, Id, Dir_to_propagate, Answers) ->
	case N of
		N_expected ->
			glob:regformat(Id) !{neighbouranswers, Answers, Dir_to_propagate};
		_ ->
			receive
				{tilevalue, SenderId, CurrentValue, Merged} ->					 
					collect(N_expected, N+1, Id, lists:map(fun(X) ->  compare_answer(X, {SenderId, CurrentValue, Merged}) end, Answers), Dir_to_propagate)
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
			case Dir of
				up -> glob:regformat(TileNo+4) ! up;
				dn -> glob:regformat(TileNo-4) ! dn;
				lx -> glob:regformat(TileNo+1) ! lx;
				rx -> glob:regformat(TileNo-1) ! rx
			end;
		true ->
			ok
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
	


format_list(L) ->
        io:format("["),
        fnl(L),
        io:format("]~n").

fnl([H]) ->
	io:format("~p", [H]);
fnl([H|T]) ->
        io:format("~p,", [H]),
	fnl(T);
fnl([]) ->
        ok.
