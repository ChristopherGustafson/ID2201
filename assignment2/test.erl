-module(test).

-export([start/0, stop/0]).

%% To set my ip
ip() -> 'sweden@130.229.147.118'.

start() ->
 	routy:start(stockholm),
 	routy:start(lund),
	routy:start(uppsala),
	routy:start(kiruna),

	stockholm ! {add, lund, {lund, ip()}},
	stockholm ! {add, uppsala, {uppsala, ip()}},
	lund ! {add, stockholm, {stockholm, ip()}},
	lund ! {add, uppsala, {uppsala, ip()}},
	lund ! {add, kiruna, {kiruna, ip()}},
	uppsala ! {add, lund, {lund, ip()}},
	uppsala ! {add, kiruna, {kiruna, ip()}},
	kiruna ! {add, uppsala, {uppsala, ip()}},
	
	stockholm ! broadcast,
    timer:sleep(100),
    lund ! broadcast,
    timer:sleep(100),
    uppsala ! broadcast,
    timer:sleep(100),
    kiruna ! broadcast,
    timer:sleep(100),
	
	stockholm ! update,
    timer:sleep(100),
    lund ! update,
    timer:sleep(100),
    uppsala ! update,
    timer:sleep(100),
    kiruna ! update,
    timer:sleep(100),

    stockholm ! {status, self()},
    timer:sleep(100),
    lund ! {status, self()},
    timer:sleep(100),
    uppsala ! {status, self()},
    timer:sleep(100),
    kiruna ! {status, self()},
    timer:sleep(100),

    timer:sleep(1000),
    stockholm ! {send, kiruna, fuckYou}.


stop() ->
	stockholm ! stop,
	lund ! stop,	
	uppsala ! stop,	
	kiruna ! stop.	