-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).


stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Clock) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      NewClock = time:inc(Name, time:merge(Clock, Time)),
      Log ! {log, Name, NewClock, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewClock);
    stop ->
      ok;
    Error -> 
      Log ! {log, Name, time, {error, Error}}
  after Wait -> 
    Selected = select(Peers),
    NewClock = time:inc(Name, Clock),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, NewClock, Message},
    jitter(Jitter),
    Log ! {log, Name, NewClock, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, NewClock)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) ->
  ok;
jitter(Jitter) ->
  timer:sleep(random:uniform(Jitter)).