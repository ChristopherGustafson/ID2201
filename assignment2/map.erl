-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).


new() ->
  [].

update(Node, Links, Map) -> 
  NewMap = lists:keydelete(Node, 1, Map),
  [{Node, Links} | NewMap].
  
  
reachable(Node, Map) -> 
  case lists:keyfind(Node, 1, Map) of
    false ->
      [];
    {_, Nodes} ->
      Nodes
    end.
  

  
all_nodes(Map) -> 
  collect_nodes(Map, []).


collect_nodes([], List) ->
  List;
collect_nodes([{Node, Adjacent} | T], List) ->
  collect_nodes(T, add_nodes([Node | Adjacent], List)).


add_nodes([], List) ->
  List;
add_nodes([H|T], List) ->
  case lists:member(H, List) of
    true ->
      add_nodes(T, List);
    false ->
      add_nodes(T, [H | List])
  end.
  
