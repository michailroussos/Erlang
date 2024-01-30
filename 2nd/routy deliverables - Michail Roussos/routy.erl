-module(routy).
-export([start/2, stop/1,print_status/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    %edw to PDF tous leei Msgs alla den exei timi auto to variable
    % kai epidi den exoume perasei to Hist fantazomai oti mallon auto thelane na valoun
    %router(Name, 0, Msgs, Intf, Table, Map).
    router(Name, 0, Hist, Intf, Table, Map).
router(Name, N, Hist, Intf, Table, Map) ->
    receive
%: %:
        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        {route, Name, From, Message} ->
            CharList = lists:map(fun(X) -> <<X>> end, Message),
            io:format("~w: received message (~s) from ~s ~n", [Name, lists:flatten(CharList), From]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~w: routing message (~w)", [Name, Message]),
            case dijkstra:route(To, Table) of
               {ok, Gw} ->
                case intf:lookup(Gw, Intf) of
                    {ok, Pid} ->
                        Pid ! {route, To, From, Message};
                    notfound ->
                        ok
                    end;
                notfound ->
                    ok 
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        {add, Node, Pid} ->
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _}  ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        stop -> 
            ok
end.


print_status(Reg)->
    Reg ! {status,self()},
    receive
        {status,{Name, N, Hist, Intf, Table, Map}}->
        io:format("Name: ~s~n", [Name]),
        io:format("Interface: ~p~n", [Intf]),
        io:format("Map: ~p~n", [Map]),
        io:format("Table: ~p~n", [Table]),
        io:format("History: ~p~n", [Hist])
    end.
