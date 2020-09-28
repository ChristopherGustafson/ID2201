-module(node1).

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
  node(Id, Predecessor, Successor).

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


node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      %io:format("Received key message~n"),
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);

    {notify, New} ->
      %io:format("Received notify message~n"),
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);

    {request, Peer} ->
      %io:format("Received request message~n"),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);

    {status, Pred} ->
      %io:format("Received status message~n"),
      Succ = stabilize(Pred, Id, Successor),
      %io:format("Succ: ~w~n", [Succ]),
      node(Id, Predecessor, Succ);

    stabilize ->
      %io:format("Node: ~w: Received stabilize message~n", [Id]),
      stabilize(Successor),
      node(Id, Predecessor, Successor);

    state ->
      io:format("Node ~w:~n My predecessor is ~w and my successor is ~w~n", [Id, Predecessor, Successor]),
      node(Id, Predecessor, Successor);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)

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

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  %io:format("Trying to send msg to ~w~n", [Spid]),
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of 
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.  


notify({Nkey, Npid}, Id, Predecessor) ->
  %io:format("notify called~n"),
  case Predecessor of
    nil ->
      {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid};
        false ->
          Predecessor
      end
    end.


create_probe(Id, {_, Spid}) ->
  T = os:timestamp(),
  Spid ! {probe, Id, [self()], T}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  io:format("Node ~w: forwarding probe~n", [Id]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, _Nodes) ->
  Time = timer:now_diff(os:timestamp(), T)/10,
  io:format("Probe took ~w micro seconds~n", [Time]).