-module(gui).

-export([display/0]).

display() ->
	register(gui,self()),
	displayloop().

% the displayloop prints out the grid,
%  then it reads a character from the standard input and acts according to the read character
displayloop() ->
	drawGrid(),
	io:fwrite("Enter input:~n"),
	{O, [V]} = io:fread(standard_io, 'input > ',"~c"),
	case O of
	ok	->
		debug:debug("You wrote ~p~n",[V]),
		case list_to_atom(V) of
		q	-> 
			io:fwrite("Quitting...~n"),
			cleanall(),
			exit(normal);
		h	->
			io:fwrite(
					"  The possible input encompasses: ~n"			++
					"    'q'            to stop the program;~n"		++
					"    'h'            to show this menu;~n"		++
					"    'w' or 'i'     to move UP;~n"				++
					"    'a' or 'j'     to move LEFT;~n"			++
					"    's' or 'k'     to move DOWN;~n"			++
					"    'd' or 'l'     to move RIGHT.~n"			++
					"  Only the first character is recognised, ~n"	++
					"  press <enter> to input commands.~n"
				),
			ok;
		w	-> 
			manager ! up;
		a -> 
			manager ! lx;
		s -> 
			manager ! dn;
		d -> 
			manager ! rx;
		i -> 
			manager ! up;
		j -> 
			manager ! lx;
		k -> 
			manager ! dn;
		l -> 
			manager ! rx;
		_		-> 
			io:fwrite("Unrecognisable input~n")
		end;
	error	->
		io:fwrite("An error occurred: ~n  ~p ~n",[V])
	end,
	displayloop().

% draws a square grid of with 4 cells per side.
	% $Len calculates the length in characters of the maximum number in the collected data
	% $Arg formats all the data properly
% ask for the data to the manager via message $senddata
drawGrid() ->
	Side = 4,
	try	manager ! sendData of
		_ ->  ok
	catch
		_:_ -> debug:debug("GUI: Cannot send to the manager.~n",[])
	end,
	Data = 
		receive
			{values, V} ->
				V;
			Othermessage ->
				debug:debug("GUI: Someone contacted the gui with an unexpected message: ~p.~n Resetting~n",[Othermessage]),
				[0,0,0,0,
				0,0,0,0,
				0,0,0,0,
				0,0,0,0]
		after
			5000->
				debug:debug("GUI not contacted for 5 seconds.~n",[]),
				[0,0,0,0,
				0,0,0,0,
				0,0,0,0,
				0,0,0,0]
		end
		,
	Len = length(integer_to_list(lists:max(Data))) + 2,
	Arg = lists:map(fun(X) -> cellFormatter(X, Len) end, Data),
	io:fwrite(
		"~s~n", [drawBar( Len , Side )]),
	io:fwrite( 
		"|~s|~s|~s|~s|~n"	++
		"|~s|~s|~s|~s|~n"	++
		"|~s|~s|~s|~s|~n"	++
		"|~s|~s|~s|~s|~n",
		Arg),
	io:fwrite(
		"~s~n", [drawBar( Len, Side )]).

% ensures that the value $P is $Len characters long for output reasons to keep the grid aligned
cellFormatter( P , Len ) ->
	Arg = 
		case P of 
			0 -> ".";
			_ -> integer_to_list(P)
		end,
	string:centre(Arg , Len).

% this function draws the top and bottom bar aligned in width with the grid
% $1 is the size of each cell while $2 is the number of cells per row
drawBar( 0 , C) ->
	"=" ++ drawCell(C); 
drawBar( N, C ) ->
	drawCell( C ) ++ drawBar( N - 1, C).

% draws a line border given that there are $1 cells in the grid
drawCell( 0 ) ->
	"";
drawCell( N ) ->
	"=" ++ drawCell( N-1 ).

%erases all known registered processes
cleanall() ->
	L = [manager, collector, lifeguard, blaster],
	lists:map(fun(X)-> cleanname(X) end, L),
	cleantile(16).
% kills a given name
cleanname( Name ) ->
	try exit(whereis(Name), cleanup) of
		_ -> ok
	catch
		_:_ -> ok
	end.
% kill all tiles
cleantile(0) ->
	ok;
cleantile(N) ->
	%debug:debug("Cleaning ~p ...",[N]),
	try	exit(whereis(glob:regformat(N)), cleanup) of
		_ -> ok
	catch
		_:_ -> ok
	end,
	cleantile(N-1).