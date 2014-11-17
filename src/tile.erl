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
			Neighbours = neighbours(Id,-4);
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

neighbours(TileNo,F)->
	[TileNo + F*I || I <- [1,2,3], TileNo + F*I > 0, TileNo + F*I < 17].

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
