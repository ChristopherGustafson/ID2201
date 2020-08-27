-module(wait).

-export([hello/0]).

hello() ->
    receive
      X -> io:format("Aaa! suprise, a message: ~s~n", [X])
    end.
