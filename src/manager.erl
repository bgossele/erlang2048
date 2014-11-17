-module(manager).

-export([manage/0]).

manage() ->
	manageloop().

% when receiving the message $senddata, spaw a collector and a broadcaster for the collection of the data
%  from the tiles. Then, once the $Data is collected, inform the lifeguard and the gui
manageloop() ->
	receive
		up ->
			Tmp = [1,2,3,4],
			lists:map(fun(X) -> glob:regformat(X) ! up end, Tmp);
		dn ->
			Tmp = [13,14,15,16],
			lists:map(fun(X) -> glob:regformat(X) ! dn end, Tmp);
		lx ->
			Tmp = [1,5,9,13],
			lists:map(fun(X) -> glob:regformat(X) ! lx end, Tmp);
		rx ->
			Tmp = [4,8,12,16],
			lists:map(fun(X) -> glob:regformat(X) ! rx end, Tmp);
		sendData ->
			Basetuple = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			% this is the instruction mentioned in the text %
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			timer:sleep(700),
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			% this is the instruction mentioned in the text %
			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			PidCollector = spawn( fun() -> collect( 0, Basetuple ) end),
			register( collector, PidCollector ),
			spawn( fun() -> broadcaster( 16, {yourValue, collector} ) end);
		{collectedData, TupleData} ->
			ListData = randomiseatile(TupleData),
			gui ! {values, ListData}
	end,
	manageloop().

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
			debug:debug("MANAGER: radomised in ~p.~n",[C1]),
			glob:regformat(C1) ! {setvalue, V1, false},
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
		Num ->
			receive
				{tilevalue, Id, Value, _, _} ->
					collect( Num+1, erlang:setelement(Id, T, Value))
			end
	end.

% Sends message $Mess to all tiles
broadcaster( 0, _ )->
	ok;
broadcaster( N, Mess ) when N < 17 -> 
	try glob:regformat(N) ! Mess of
		_ -> 
			debug:debug("broadcasting to ~p.~n",[N]),
			ok
	catch
		_:F -> 
			debug:debug("BROADCASTER: cannot commmunicate to ~p. Error ~p.~n",[N,F])
	end,
	broadcaster( N-1, Mess ).
