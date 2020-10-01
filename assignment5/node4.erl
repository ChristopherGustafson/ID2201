-module(node4).

-export([start/1, start/2]).

-define(Stabilize, 100).
-define(Timeout, 1000).



start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  Next = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, Next, storage:create(), storage:create()).

connect(Id, nil)->
  {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Ref = monitor(Peer),
      {ok, {Skey, Ref, Peer}}
  
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
  %io:format("Trying to send msg to ~w~n", [Spid]),
  Spid ! {request, self()}.


node(Id, Predecessor, Successor, Next, Store, Replica) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {notify, New} ->
      {Pred, Sto, Repl} = notify(New, Id, Predecessor, Store, Replica),
      node(Id, Pred, Successor, Next, Sto, storage:merge(Replica, Repl));

    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Nxt, Store, Replica);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
                  Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Added, Replica);
      
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {handover, Elements, Repl} ->
      
      Merged = storage:merge(Store, Elements),
      MergedRepl = storage:merge(Replica, Repl),
      node(Id, Predecessor, Successor, Next, Merged, MergedRepl);

    {replicate, Key, Value} ->
      node(Id, Predecessor, Successor, Next, Store, storage:add(Key, Value, Replica));

    {replica, Repl} ->
      node(Id, Predecessor, Successor, Next, Store, Repl);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    state ->
      io:format("~nNode ~w:~n My predecessor is ~w~n My successor is ~w~n My next is ~w~n", [{Id,self()}, Predecessor, Successor, Next]),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt, Sto} = down(Ref, Predecessor, Successor, Next, Store, Replica),
      node(Id, Pred, Succ, Nxt, Sto, Replica)

    end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
  case Predecessor of
    nil ->
      {Keep, Repl} = handover(Id, Store, Replica, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep, Repl};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Keep, Repl} = handover(Id, Store, Replica, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep, Repl};
        false ->
          {Predecessor, Store, Replica}
      end
    end.

handover(Id, Store, Replica, Nkey, Npid) ->  
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Rest, Replica},
  {Keep, Rest}.

request(Peer, Predecessor, {Skey, _, Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
  end.

stabilize(Pred, Next, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Id,  _} ->
      {Successor, Next};
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};

    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          drop(Sref),
          Xref = monitor(Xpid),   
          {{Xkey, Xref, Xpid}, {Skey, Spid}};
        false ->
          Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      Spid ! {replicate, Key, Value},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.


lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

% My predecessor has died
down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  io:format("~nmy pred has died~n"),
  {nil, Successor, Next, storage:merge(Store, Replica)};

% My sucessor has died      
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, _Replica) ->
  io:format("~nmy succ has died~n"),
  Nref = monitor(Npid),
  stabilize({Nkey, Nref, Npid}),

  Npid ! {replica, Store},
  {Predecessor, {Nkey, Nref, Npid}, nil, Store}.


create_probe(Id, {_, _, Spid}) ->
  T = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [self()], T}.

forward_probe(Ref, T, Nodes, Id, {_, _, Spid}) ->
  io:format("Node ~w: forwarding probe~n", [Id]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, _Nodes) ->
  Time = (erlang:system_time(micro_seconds) - T) / 1000000,
  io:format("Probe took ~w seconds~n", [Time]).


monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).