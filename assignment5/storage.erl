-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value} | Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  split(From, To, Store, [], []).

split(_From, _To, [], Updated, Rest) ->
  {Updated, Rest};
split(From, To, [{Key, Value} | Tail], Updated, Rest) ->
  case key:between(Key, From, To) of
    true ->
      split(From, To, Tail, [{Key, Value} | Updated], Rest);
    false ->
      split(From, To, Tail, Updated, [{Key, Value} | Rest])
  end.

merge(Entries, Store) ->
  lists:append(Store, Entries).
