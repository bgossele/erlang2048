-module(blaster).

-export([blast/0]).

% initialise the random function
blast()->
	{V1,V2,V3} = now(),
    random:seed(V1, V2, V3),
    register(blaster,self()),
    timer:sleep(1200),			% let the game start
    blastloop(25).				% then blast

% receive loop of the blaster
% if it receives a message, it stops
%	otherwise, on a 1/$Chance of cases, every 150 msec, it randomises a tile and kills it
blastloop( Chance )->
	receive
		_ -> ok
	after
		150 ->
    	   	R = random:uniform(Chance),
    	   	case R of
    			2 ->
    				Num = glob:regformat(random:uniform(16)),		
    				%debug:debug
    				io:format("BLASTER: Killing tile: ~p! ~n",[Num]),
    				case whereis(Num) of
						undefined ->
							debug:debug("BLASTER: Could not locate tile ~p.~n",[Num]);
    					Pid -> 
    						%try exit(Pid, blasted) of
    						try Pid ! die of
								_ ->
									try unregister(Num) of
										_ -> 
											ok
									catch
										error:_ ->
											debug:debug("BLASTER: Could not unregister name ~p.~n",[Num])
									end
							catch
								error:_ ->
									debug:debug("BLASTER: Could not kill tile ~p with Pid ~p.~n",[Num,Pid])
							end
					end;
    			_ -> 
    				%debug:debug(".",[]),
    				ok
    		end,
    		blastloop( Chance )
	end.