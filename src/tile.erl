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
			exit(killed);
		up ->
			Neighbours = neighbours(Id,-4),
			Neigbhour_values = lists:map(fun askNeighbour/1, Neighbours);
		%reeds gemergde eruitfilteren
		%zolang eerste entries value 0 hebben, entry met == value zoeken, daarmee mergen
		%anders naar verste met value 0
		%anders niets
		%
		dn ->
			Neighbours = neighbours(Id,4);
		lx ->
			Neighbours = neighbours(Id,-1);
		rx ->
			Neighbours = neighbours(Id,1);
		{yourValue, Repl} ->
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			ok
	end,
	tilelife(Id, 42, true).

askNeighbour(Id)->
	glob:regformat(Id) ! {yourValue, self()},
	receive
		{tileValue, Id, CurrentValue, Merged} ->
			debug:debug("Tile ~p, value ~p, ~p", [Id, CurrentValue, Merged]),
			[Id, CurrentValue, Merged]
	end.
 
neighbours(TileNo,F)->
	[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17].

not_end_of_board(Dir, TileNo)->
	case Dir of
		up ->
			TileNo < 13;
		dn ->
			TileNo > 4;
		lx ->
			TileNo rem 4 /= 0;
		rx ->
			(TileNo - 1) rem 4 /= 0
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
