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
					%Neighbours = neighbours(Id,-4),
					%collector spawnen
					%vraag lanceren
					%Neighbour_values = lists:map(fun askNeighbour/1, Neighbours),
					%itereren over neighbours en checken in map. Laatst gecheckte bijhouden. Iets is een match als == 0 of zelfde value en niet gemerged.
					%Als geen match, mergen met vorige neighbour.
					ok
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
		{neighbouranswers, M} ->
			%itereren over neighbours en checken in map. Laatst gecheckte bijhouden. Iets is een match als == 0 of zelfde value en niet gemerged.
			%Als geen match, mergen met vorige neighbour.
			NextValue = CurrentValue,
			NextMerged = Merged
	end,
	tilelife(Id, NextValue, NextMerged).

askNeighbour(Id)->
	glob:regformat(Id) ! {yourValue, self()},
	receive
		{tileValue, Id, CurrentValue, Merged} ->
			io:format("Tile ~p, value ~p, ~p", [Id, CurrentValue, Merged]),
			[Id, CurrentValue, Merged]
	end.
 
neighbours(TileNo,F)->
	[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17].
	
collect(N_expected, N, Id, M) ->
	case N of
		N_expected ->
			glob:regformat(Id) ! {neighbouranswers, M};
		_ ->
			receive
				{tilevalue, SenderId, CurrentValue, Merged} ->					 
					collect(N_expected, N+1, Id, maps:put(SenderId, {CurrentValue, Merged}, M))
			end
	end.
	
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
