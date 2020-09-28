-module(test).

-export([run/2]).

run(N, Node) ->
  Keys = add(N, Node),
  Start = erlang:system_time(micro_seconds),
  lookup(Keys, Node),
  Finish = erlang:system_time(micro_seconds),
  T = (Finish - Start) / 1000000,
  io:format("Execution time: ~w seconds~n", [T]).


add(0, _) ->
  [];
add(N, Node) ->
  Key = key:generate(),
  Ref = make_ref(),
  Node ! {add, Key, test, Ref, self()},
  receive
    {Ref, ok} ->
      [Key | add(N-1, Node)]
  end.

lookup([], _) ->
  ok;
lookup([Key | Rest], Node) ->
  Ref = make_ref(),
  Node ! {lookup, Key, Ref, self()},
  receive
    {Ref, Result} ->
      io:format("Received lookup result: ~w~n", [Result]),
      lookup(Rest, Node)
  end.  
  
