-module(node2).

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
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil)->
  {ok, {Id, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  %io:format("Trying to send msg to ~w~n", [Spid]),
  Spid ! {request, self()}.


node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      %io:format("Received key message~n"),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    {notify, New} ->
      %io:format("Received notify message~n"),
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Sto);

    {request, Peer} ->
      %io:format("Received request message~n"),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);

    {status, Pred} ->
      %io:format("Received status message~n"),
      Succ = stabilize(Pred, Id, Successor),
      %io:format("Succ: ~w~n", [Succ]),
      node(Id, Predecessor, Succ, Store);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
                  Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
      
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);

    stabilize ->
      %io:format("Node: ~w: Received stabilize message~n", [Id]),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);

    state ->
      io:format("Node ~w:~n My predecessor is ~w and my successor is ~w~n", [Id, Predecessor, Successor]),
      node(Id, Predecessor, Successor, Store);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store)

    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  %io:format("notify called~n"),
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
    end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Rest},
  Keep.

request(Peer, Predecessor) ->
  case Predecessor of 
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.  


stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.


lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.














create_probe(Id, {_, Spid}) ->
  T = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [self()], T}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  io:format("Node ~w: forwarding probe~n", [Id]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, _Nodes) ->
  Time = (erlang:system_time(micro_seconds) - T) / 1000000,
  io:format("Probe took ~w seconds~n", [Time]).