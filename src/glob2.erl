% Author: Brecht Gosselé, r0259849

-module(glob2).

-export([sendToTile/2]).

sendToTile(Id, Message) ->
	try
		glob:regformat(Id) ! Message
	of
		_ -> ok
	catch
		error:_ -> 
			debug:debug("error sending to ~p~n", [Id]),
			sendToTile(Id, Message)
	end.
