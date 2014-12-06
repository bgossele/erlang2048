-module(main).

-export([play/0,playnoblaster/0]).

play() ->
	P = spawn(fun manager:manage/0),	% create the manager and register it
	glob:registerName(manager,P),			%timer:sleep(200),
	spawn(fun blaster:blast/0), 		% create the blaster
	gui:display().						% continue as a gui


playnoblaster() ->
	P = spawn(fun manager:manage/0),
	glob:registerName(manager,P),			
	gui:display().	