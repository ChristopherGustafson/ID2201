-module(parTest).

-export([bench/2]).


bench(Host, Port) ->
    bench(5, Host, Port).

bench(0, _, _) ->
  ok;
bench(Rounds, Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    spawnRequesters(16, 150, Host, Port, self()),
    collectRequesters(16),
    Finish = erlang:system_time(micro_seconds),
    T = (Finish - Start) / 1000000,
    io:format("Execution time: ~w seconds~n", [T]),
    bench(Rounds-1, Host, Port).


spawnRequesters(0, _, _, _, _) -> ok;
spawnRequesters(Requesters, N, Host, Port, Main) ->
    spawn(fun() -> runRequests(N, Host, Port, Main) end),
    spawnRequesters(Requesters-1, N, Host, Port, Main).

runRequests(N, Host, Port, Main) ->
  run(N, Host, Port),
  Main ! ok.


collectRequesters(0) ->
  ok;
collectRequesters(N) ->
  receive
    ok ->
      collectRequesters(N-1)
  end.

run(0, _, _) ->
  ok;
run(N, Host, Port) ->
    request(Host, Port),
    run(N - 1, Host, Port).

    
request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
      {ok, _} -> 
        ok;
      {error, Error} ->
	      io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
