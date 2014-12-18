% Author: Brecht Gosselé, r0259849

-module(manager).

-export([manage/0]).

manage() ->
	Tmp = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],
	lists:map(fun(X) -> launchtile(X, 0, false) end, Tmp),
	process_flag(trap_exit, true),
	manageloop(16, false).

% when receiving the message $senddata, spawn a collector and a broadcaster for the collection of the data
%  from the tiles. Then, once the $Data is collected, inform the lifeguard and the gui
manageloop(TilesReady, SendDataRequestPending) ->
	receive
		up ->
			Tmp = [1,2,3,4],
			NewTilesReady = TilesReady,
			SDRP = SendDataRequestPending,
			lists:map(fun(X) -> glob:sendToTile(X, up) end, Tmp);
		dn ->
			Tmp = [13,14,15,16],
			NewTilesReady = TilesReady,
			SDRP = SendDataRequestPending,
			lists:map(fun(X) -> glob:sendToTile(X, dn) end, Tmp);
		lx ->
			Tmp = [1,5,9,13],
			NewTilesReady = TilesReady,
			SDRP = SendDataRequestPending,
			lists:map(fun(X) -> glob:sendToTile(X, lx) end, Tmp);
		rx ->
			Tmp = [4,8,12,16],
			NewTilesReady = TilesReady,
			SDRP = SendDataRequestPending,
			lists:map(fun(X) -> glob:sendToTile(X, rx) end, Tmp);
		tileReady ->
			case (TilesReady == 15) andalso (SendDataRequestPending) of
				true ->
					NewTilesReady = 0,
					SDRP = false,
					launchcollector();
				false ->
					NewTilesReady = TilesReady + 1,
					SDRP = SendDataRequestPending
			end;					
		sendData ->
			case TilesReady == 16 of
				true ->
					NewTilesReady = 0,
					SDRP = false,
					launchcollector();
				false ->
					NewTilesReady = TilesReady,
					SDRP = true
			end;		
		{collectedData, TupleData} ->
			ListData = randomiseatile(TupleData),
			gui ! {values, ListData},
			SDRP = SendDataRequestPending,
			NewTilesReady = TilesReady;
		{'EXIT',_,{killed, Id, Value, Merged}} ->
			launchtile(Id, Value, Merged),
			SDRP = SendDataRequestPending,
			NewTilesReady = TilesReady 
	end,
	manageloop(NewTilesReady, SDRP).

launchtile(Id, Value, Merged) ->
	glob:registerName(glob:regformat(Id),spawn_link(tile, tilelife, [Id, Value, Merged])).

launchcollector() ->
	spawn( fun() -> broadcaster( 16, sync) end), %Notify tiles that round is over.
	Basetuple = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	PidCollector = spawn( fun() -> collect( 0, Basetuple ) end),
	glob:registerName( collector, PidCollector ),
	spawn( fun() -> broadcaster( 16, {yourValue, collector} ) end).

% takes a tuple of data in input and returns it in a list format
% with two elements that were at 0 now randomised at 2
randomiseatile( Tuple )->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	case glob:zeroesintuple(Tuple) of
		0 ->
			Tu = Tuple;
		_ ->
			C1 = getCand(0, Tuple),
			V1 = 2,
			%debug:debug("MANAGER: randomised in ~p.~n",[C1]),
			glob:sendToTile(C1, {setvalue, V1, false}),
			Tu = erlang:setelement(C1,Tuple,V1)
	end,
	erlang:tuple_to_list(Tu).

% returns a number from 1 to 16 different from $Oth
	% such that its value in $T is 0, i.e. $return can be initialised at random
getCand( Oth , T)->
	C = random:uniform(16),
	case C of
		Oth -> getCand(Oth, T);
		_ ->
			case erlang:element(C, T) of
				0 -> C;
				_ -> getCand(Oth, T)
			end
	end.

% collects 16 numbes in $T, then returns the related tuple
%	$T is a tuple of length 16
collect( N , T) ->
	case N of
		16 -> 
			manager ! {collectedData, T};
		_ ->
			receive
				{tilevalue, Id, Value, _} ->
					collect( N+1, erlang:setelement(Id, T, Value))
			end
	end.

% Sends message $Mess to all tiles
broadcaster( 0, _ )->
	ok;
broadcaster( N, Mess ) when N < 17 -> 
	try glob:sendToTile(N, Mess) of
		_ -> 
			%debug:debug("broadcasting to ~p.~n",[N]),
			ok
	catch
		_:F -> 
			debug:debug("BROADCASTER: cannot commmunicate to ~p. Error ~p.~n",[N,F])
	end,
	broadcaster( N-1, Mess ).
