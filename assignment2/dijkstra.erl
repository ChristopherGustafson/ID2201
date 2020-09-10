-module(dijkstra).

-export([table/2, route/2, iterate/3]).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_, Gateway} ->
      {ok, Gateway};
    false ->
      {error, notfound}
    end.

table(Gateways, Map) ->
  InitialList = lists:map(fun(Node) -> {Node, inf, unkown} end, map:all_nodes(Map)),
  Sorted = lists:foldl(fun(Gateway, Sorted) -> update(Gateway, 0, Gateway, Sorted) end, InitialList, Gateways),
  iterate(Sorted, Map, []).


iterate([], _, Table) ->
  Table;
iterate([{_, inf, _} | _], _, Table) ->
  Table;
iterate([{Node, Length, Gateway} | Tail], Map, Table) ->
  Reachable = map:reachable(Node, Map),
  Rest = lists:foldl(fun(Elem, Sorted) -> update(Elem, Length+1, Node, Sorted) end, Tail, Reachable),
  iterate(Rest, Map, [{Node, Gateway} | Table]).


update(Node, N, Gateway, Sorted) ->
  L = entry(Node, Sorted),
  if 
    N < L -> 
      replace(Node, N, Gateway, Sorted);
    true ->
      Sorted
    end.

entry(_, []) ->
  0;
entry(Node, [{Dest, Length, _} | Tail]) ->
  case Dest of
    Node ->
      Length;
    _ ->
      entry(Node, Tail)
    end. 

replace(Node, N, Gateway, Sorted) ->
  Unsorted = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
  lists:keysort(2, Unsorted).
