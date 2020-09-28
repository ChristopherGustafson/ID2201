-module(loggy).

-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, HoldBack) ->
  receive
    {log, From, Time, Msg} ->
      NewClock = time:update(From, Time, Clock),
      NewHoldBack = checkHoldBack([{From, Time, Msg} | HoldBack], NewClock),
      %io:format("Logger: Adding msg to queue, length: ~w~n", [len(NewHoldBack)]),
      loop(NewClock, NewHoldBack);
    stop ->
      ok
    end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).


checkHoldBack([], _) ->
  [];
checkHoldBack([{From, Time, Msg} | Rest], Clock) ->
  case time:safe(Time, Clock) of
      true ->
        log(From, Time, Msg),
        checkHoldBack(Rest, Clock);
      false ->
        [{From, Time, Msg} | checkHoldBack(Rest, Clock)]
  end.

len(List)->
  len(List, 0).

len([], Count) ->
  Count;
len([_ | Rest], Count) ->
  len(Rest, Count+1).