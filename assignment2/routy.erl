-module(routy).

-export([start/1, stop/1, status/0]).

start(Name) ->
  register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Interface = interface:new(),
  Map = map:new(),
  Table = dijkstra:table(Interface, Map),
  History = history:new(Name),
  router(Name, 0, History, Interface, Table, Map).

status() ->
  receive
    {status, {Name, N, History, Interface, Table, Map}} ->
      io:format("The status of ~w:~n  counter: ~w~n", [Name, N])
    end.


router(Name, N, History, Interface, Table, Map) ->
  receive
    {links, Node, R, Links} ->
      case history:update(Node, R, History) of
        {new, Hist1} ->
          interface:broadcast({links, Node, R, Links}, Interface),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Interface, Table, Map1);
        old ->
          router(Name, N, History, Interface, Table, Map)
        end;

    update ->
      Table1 = dijkstra:table(interface:list(Interface), Map),
      router(Name, N, History, Interface, Table1, Map);

    broadcast -> 
      io:format("~w: Broadcasting~n", [Name]),
      Message = {links, Name, N, interface:list(Interface)},
      interface:broadcast(Message, Interface),
      router(Name, N+1, History, Interface, Table, Map);

    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
      Intf1 = interface:add(Node, Ref, Pid, Interface),
      io:format("~w: Added ~w to interface ~n", [Name, Node]),
      router(Name, N, History, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = interface:ref(Node, Interface),
      erlang:demonitor(Ref),
      Intf1 = interface:remove(Node, Interface),
      router(Name, N, History, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = interface:name(Ref, Interface),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      Intf1 = interface:remove(Down, Interface),
      router(Name, N, History, Intf1, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, History, Interface, Table, Map}},
      router(Name, N, History, Interface, Table, Map);

    {route, Name, From, Message} ->
      io:format("~w: received message ~w~n", [Name, Message]),
      router(Name, N, History, Interface, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w) to ~w~n", [Name, Message, To]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case interface:lookup(Gw, Interface) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
            end;
          notfound ->
            ok
          end,
      router(Name, N, History, Interface, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, History, Interface, Table, Map);
     
    stop ->
      ok
    end.

