-module(history).

-export([new/1, update/3]).

new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    {Node, MsgNumber} ->
      if
        N < MsgNumber ->
          old;
        true ->
          Updated = lists:keyreplace(Node, 1, History, {Node, N}),
          {new, Updated}
      end;
    false ->
      {new, [{Node, N} | History]}
    end.