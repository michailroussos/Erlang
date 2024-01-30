-module(node1).
-export([start/1,start/2]).
-define(Timeout, 100000).
-define(Stabilize, 1000).
-import(key, [between/3,generate/0]).

%-compile(export_all).
%-define(timeout, 5000).
%-define(arghh, 200).

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

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
%if we get an unexpected message we print it here
        Unexpected_Message->
            io:format("Node Id~w got unexpected Message on init: ~w ~n", [Id,Unexpected_Message]),
            node(Id, Predecessor, Successor)
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
                    stabilize(Pred, Id, {Xkey, Xpid});
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
%If our own predecessor is set to nil the case is closed  
% (probably set the new node as our predecessor)
        nil -> 
            %:
            Npid ! {status,{Nkey,Npid}},
			{Nkey,Npid};
%if we already have a predecessor we of course have to check if 
% the new node actually should be our predecessor or not. 
% Do we need a special case to detect that we’re pointing to ourselves?
% Do we have to inform the new node about our decision? How will it know 
% if we have discarded its friendly proposal?
        {Pkey,  Ppid} ->
            case between(Nkey, Pkey, Id) of
                true ->
					Npid ! {status,{Nkey,Npid}},
					{Nkey,Npid};
				false ->
					Npid ! {status,{Pkey, Ppid}},
					{Pkey, Ppid}
            end 
    end.


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
        io:format("Time out: no response~n",[])
end.

%send a new probe to succ
create_probe(Id, Successor)->
    {_,Pid}=Successor,
    Pid ! {probe, Id,[Id],erlang:system_time(micro_seconds)}.

%print Time
remove_probe(T, Nodes) ->
	Duration = timer:now_diff(erlang:system_time(micro_seconds), T),
	List = lists:flatlength(Nodes),
	io:format("Node:~w Received the probe that I created before ~w 
    microseconds ~n. The network is: ~w~n~n", [self(), Duration, List]).

%forward the probe to the successor
forward_probe(Ref, T, Nodes, Id, Successor)->
	{_,Pid} = Successor,
	Pid ! {probe,Ref,Nodes++[Id],T}.



