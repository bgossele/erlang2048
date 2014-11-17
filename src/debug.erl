-module(debug).

-export([debug/2,simp/0]).

%centralised debug function, for either writing to a log or to the console
	% surrently it prints to screen
debug( S, L ) ->
	case isdebug() of
		true ->	io:format(S, L);
		false -> ok
	end.

% change this to enable debugging
isdebug() -> false.


%%%%%%%%%%%%%%%%%%%%%%%
% some test functions %
%%%%%%%%%%%%%%%%%%%%%%%

simp() ->
	register(glob:regformat(2), self()),
	receive
		X -> X
	end.