-module(node2).
-export([start/1,start/2]).
%-compile(export_all).
-define(Timeout, 1000).
-define(Stabilize, 1000).
-import(key, [between/3,generate/0]).


%-export([start/1,start/2]).

%-define(timeout, 5000).
%-define(arghh, 200).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    %io:format("Init of Node Id ~w and peer: ~w ~n", [Id, Peer]),
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store=storage:create(),
    node(Id, Predecessor, Successor,Store).

node(Id, Predecessor, Successor,Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            %io:format("Node ~w got {key, Qref, Peer} ~w, ~w~n", [Id,Qref, Peer]),
            node(Id, Predecessor, Successor, Store);
        %change Predecessor and Store if needed
        {notify, New} ->
            {NewPredecessor,NewStore} = notify(New, Id, Predecessor,Store),
            %io:format("Node ~w got {notify, New} ~w~n", [Id,New]),
            node(Id, NewPredecessor, Successor, NewStore);
        %make req to pred
        {request, Peer} ->
            request(Peer, Predecessor),
            %io:format("Node ~w got {request, Peer} ~w~n", [Id,Peer]),
            node(Id, Predecessor, Successor, Store);
        %update Successor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            %io:format("Node ~w got {status, Pred} ~w~n", [Id,Pred]),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            %io:format("Node ~w got stabilize ~n", [Id]),
            node(Id, Predecessor, Successor, Store);
        probe ->
            create_probe(Id, Successor),
            %io:format("Node ~w got probe ~n", [Id]),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            %io:format("Node ~w got {probe, Id, Nodes, T} ~w,~w,~w~n", [Id,Id, Nodes, T]),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            %io:format("Node ~w got {probe, Id, Nodes, T} ~w,~w,~w~n", [Id,Ref, Nodes, T]),
            node(Id, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            %io:format("Node ~w got {add, Key, Value, Qref, Client} ~w,~w,~w,~w~n", [Id,Key, Value, Qref, Client]),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            %io:format("Node ~w got {lookup, Key, Qref, Client} ~w,~w,~w~n", [Id,Key, Qref, Client]),
            node(Id, Predecessor, Successor, Store);
        %simple add
        {add, Key, Value}->
            %io:format("Node ~w got {add, Key, Value} ~w,~w~n", [Id,Key, Value]),
			self() ! {add, Key, Value, self(), self()};
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            %io:format("Node ~w got {handover, Elements} ~w~n", [Id,Elements]),
            node(Id, Predecessor, Successor, Merged);
        status ->
			io:format("NodeID= ~w ~nPredecessor=~w ~nSuccessor=~w  ~nStorage: ~w~n", [Id,Predecessor,Successor, Store]),
			node(Id, Predecessor, Successor, Store);
%if we get an unexpected message we print it here
        Unexpected_Message->
            io:format("Node Id~w got unexpected Message on init: ~w ~n", [Id,Unexpected_Message]),
            node(Id, Predecessor, Successor, Store)
end.

stabilize(Pred, Id, Successor) ->
    
    {Skey, Spid} = Successor,
    %The Pred argument is our successor’s current predecessor. 
    case Pred of
    %If this i nil, we should inform it about our existence.
        nil -> 
            %:
            Spid ! {notify,{Id,self()}},
            Successor;
    %If it points back to us, we don’t have to do anything.
        {Id, _} -> 
            %:
            Successor;
    %If it is pointing to itself, we should notify it about our existence.
        {Skey, _} -> 
            %:
            Spid ! {notify,{Id,self()}},
            Successor;
    % If it’s pointing to another node, we need to be careful.             
        {Xkey, Xpid} ->
% we need to check the relation
            case between(Xkey, Id, Skey) of
                true -> 
                    %stabilize(Pred, Id, {Xkey, Xpid});
                    Xpid ! {request, self()}, 
					{Xkey, Xpid};
                false -> 
                    Spid ! {notify,{Id,self()}},
                    Successor
            end 
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            %io:format("Entered Notify for nil, Node ~w : Nkey ~w ,Npid ~w , Id ~w , Predecessor ~w , Store ~w~n", [Id,Nkey,Npid, Id, Predecessor, Store]),
            Keep = handover(Id, Store, Nkey, Npid),
            %: 
            %io:format("Notify, Node ~w : after handover Keep ~w~n", [Id, Keep]),

            {{Nkey,Npid},Keep};
        {Pkey, _} ->
            %io:format("Notify Node ~w, not nil  Nkey ~w, Pkey ~w, Store ~w~n", [Id,Nkey, Pkey, Store]),
            %io:format("Notify Node ~w, Nkey ~w, Pkey ~w, Id ~w~n", [Id,Nkey, Pkey, Id]),

            case between(Nkey, Pkey, Id) of
                true ->
                    %:
                    %: 
                    Keep = handover(Id, Store , Nkey , Npid),
                    %io:format("Notify not nil, true Nkey ~w, Pkey ~w, Id ~w~n", [Nkey, Pkey, Id]),
					{{Nkey,Npid},Keep};
                false ->
                    %: 
                    %io:format("Notify not nil, false Nkey ~w, Pkey ~w, Id ~w~n", [Nkey, Pkey, Id]),
                    {Predecessor,Store}
            end
    end.


handover(Id, Store, Nkey, Npid) ->
    %{...,...} = split(Id, Nkey, Store),
    {Rest,Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

connect(Id, nil) ->
    %{ok, .......};
    {ok, {Id,self()}};

connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            %:
            {ok, {Skey,Peer}}
    after ?Timeout ->
        io:format("Time out in the connect!~n",[])
end.

%send a new probe to succ
create_probe(Id, Successor)->
    {_,Pid}=Successor,
    %Pid ! {probe, Id,[Id],erlang:system_time(micro_seconds)}.
    Pid ! {probe, Id,[Id],erlang:now()}.


%print Time
remove_probe(T, Nodes) ->
	Duration = timer:now_diff(erlang:now(), T),
    %Duration = timer:now_diff(erlang:system_time(micro_seconds), T),
	List = lists:flatlength(Nodes),
	io:format("Node:~w Received the probe that I created before ~w microseconds ~n. The network is: ~w, Nodes:~w~n", [self(), Duration, List, Nodes]).

%forward the probe to the successor
forward_probe(Ref, T, Nodes, Id, Successor)->
	{_,Pid} = Successor,
	Pid ! {probe,Ref,Nodes++[Id],T}.


add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    %To add a new key value, we must first determine if our node is the 
    % node that should take care of the key. A node will take care of all 
    % keys from (but not including) the identifier of its predecessor to 
    % (and including) the identifier of itself. If we are not responsible, 
    % we send an add message to our successor.    
    case between(Key, Pkey, Id) of
    true ->
        Client ! {Qref, ok},
        %:
        storage:add(Key, Value, Store);
    false ->
        %:
        %:
        Spid ! {add, Key, Value, Qref, Client},
        Store 
    end.


lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            %:
            Spid ! {lookup, Key, Qref, Client}
    end.
