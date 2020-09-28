-module(key).

-export([between/3, generate/0]).

-define(maxKey, 1000000000).

generate() ->
  rand:uniform(?maxKey).


between(Key, From, To) when To > From ->
  if 
    Key > From ->
      if
        Key =< To ->  
          true;
        true ->
          false
        end;
    true ->
      false
    end;
% From is bigger than to, thus we are checking an interval that wraps around from max to 0
between(Key, From, To) when From > To ->
  if
    Key > From ->
      true;
    Key < To ->
      true;
    true ->
      false
  end;
% From == To which represents the full circle, key is always between
between(_, From, To) when From == To ->
  true.