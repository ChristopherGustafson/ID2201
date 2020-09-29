-module(node3).

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
  node(Id, Predecessor, Successor, Next, storage:create()).

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


node(Id, Predecessor, Successor, Next, Store) ->
  receive
    {key, Qref, Peer} ->
      %io:format("Received key message~n"),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store);

    {notify, New} ->
      %io:format("Received notify message~n"),
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Next, Sto);

    {request, Peer} ->
      %io:format("Received request message~n"),
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store);

    {status, Pred, Nx} ->
      %io:format("Received status message~n"),
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      %io:format("Succ: ~w~n", [Succ]),
      node(Id, Predecessor, Succ, Nxt, Store);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
                  Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Added);
      
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Next, Merged);

    stabilize ->
      %io:format("Node: ~w: Received stabilize message~n", [Id]),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store);

    state ->
      io:format("Node ~w:~n My predecessor is ~w~n My successor is ~w~n My next is ~w~n", [{Id,self()}, Predecessor, Successor, Next]),
      node(Id, Predecessor, Successor, Next, Store);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Next, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Next, Store);

    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Nxt, Store)

    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  %io:format("notify called~n"),
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
    end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Rest},
  Keep.

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


down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  Nref = monitor(Npid),
  Npid ! {request, self()},
  {Predecessor, {Nkey, Nref, Npid}, nil}.







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