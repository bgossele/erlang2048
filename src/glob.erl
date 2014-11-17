-module(glob).

-export([regformat/1,registerName/2,indexof/2,zeroesintuple/1]).

% centralised register function, to ensure that registered names have the same format
regformat( Number ) ->
	list_to_atom(integer_to_list(Number)).

% removes a registered name and then registers it
registerName(Name, Pid) ->
	case lists:member( Name, registered()) of
		true ->
			unregister(Name);
		false->
		  ok
	end,
	register(Name, Pid).

% returns the index of $Elem in $List
indexof(Elem, L) ->
	indexof(Elem, L, 1).
indexof(_, [], _) ->
	notfound;
indexof(Elem, [H|T], Index) ->
	case Elem of
		H -> 
			Index;
		_ ->
			indexof(Elem, T, Index +1)
	end.

% returns the number of zeroes in a tuple
zeroesintuple(T) ->
	zeroesintuple(T, tuple_size(T), 0).
zeroesintuple( _, 0, Acc )->
	Acc;
zeroesintuple( T, N, Acc)->
	case erlang:element(N, T) of
		0 -> zeroesintuple(T, N-1, Acc+1);
		_ -> zeroesintuple(T, N-1, Acc)
	end.