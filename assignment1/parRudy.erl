-module(parRudy).

-export([start/2, stop/0]).

start(Port, N) ->
    register(rudy, spawn(fun () -> init(Port, N) end)).

stop() -> rudy ! stop.

init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}, {backlog, 100}],
    case gen_tcp:listen(Port, Opt) of
      {ok, Listen} ->
	      spawnHandlers(Listen, N), 
        super();
      {error, Error} ->
	      io:format("rudy: error: ~w~n", [Error])
    end.

super() ->
  receive
    stop ->
      ok
    end.

spawnHandlers(Listen, N) ->
    case N of
      0 -> 
        ok;
      N ->
	      spawn(fun () -> handler(Listen) end),

	      spawnHandlers(Listen, N - 1)
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
      {ok, Client} -> 
        request(Client),
        handler(Listen);
      {error, Error} ->
	      io:format("rudy: error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
      {ok, Str} ->
	  Parsed = http:parse_request(Str),
	  Response = reply(Parsed),
	  gen_tcp:send(Client, Response);
      {error, Error} ->
	  io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("Replying request for " ++ URI).
