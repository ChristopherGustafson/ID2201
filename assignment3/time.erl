-module(time).

-export([zero/0, inc/2, merge/2, clock/1, update/3, safe/2]).


zero() ->
  0.

inc(_Name, T) ->
  T+1.

merge(Ti, Tj) ->
  if
    Ti > Tj ->
      Ti;
    true ->
      Tj
    end.

leq(Ti, Tj) ->
  if
    Tj < Ti ->
      false;
    true ->
      true
    end.

clock(Nodes) ->
  lists:map(fun(Node) -> {Node, zero()} end, Nodes).

update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
  [{_, LowestTime} | _] = lists:keysort(2, Clock),
  leq(Time, LowestTime).
