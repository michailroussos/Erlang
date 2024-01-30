-module(gms3).
-export([start/1,start/2]).

-define(timeout, 5000).
-define(arghh, 200).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    io:format("Init Leader ID~w ~n", [Id]),
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
    io:format("Init Slave ID~w ~n", [Id]),
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view,N ,[Leader|Slaves], Group} ->
            io:format("Slave ID~w received the View~n", [Id]),
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N+1,{view,N ,[Leader|Slaves], Group}, Slaves, Group);
        Unexpected_Message->
            io:format("Slave ID~w got unexpected Message on init: ~w ~n", [Id,Unexpected_Message])
    after ?timeout ->
        io:format("Init of Slave ID~w timed out~n", [Id]),
        Master ! {error, "no reply from leader"}
end.

%the slave procedure is extended with two arguments: N and Last.
% N is the expected sequence number of the next message, 
% and Last is a copy of the last message (a regular message or a view) 
% received from the leader.
slave(Id, Master, Leader, N, Last, Slaves, Group)->
    io:format("Slave ID~w start with N ~w ~n", [Id,N]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Msg, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        %% here we skip older messages
        {msg, I, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        %{msg, N, Msg} ->
            %Master ! Msg,
            %slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
        {msg, NewN, Msg} ->
            Master ! Msg,
            bcast(Id, {msg, NewN, Msg}, lists:delete(self(),Slaves)),
            slave(Id, Master, Leader, NewN+1, {msg, NewN, Msg}, Slaves, Group);
        %{view, N, [Leader|Slaves2], Group2} ->
            %Master ! {view, Group2},
            %slave(Id, Master, Leader, N, {view, N+1, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {view, NewN, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            bcast(Id, {view, NewN, [Leader|Slaves2], Group2}, lists:delete(self(),Slaves)),
            slave(Id, Master, Leader, NewN+1, {view, NewN, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop -> 
            ok;
        Unexpected_Message->
            io:format("Slave ID~w , got unexpected Message: ~w ~n, current message ~w, current leader~w ~n", [Id,Unexpected_Message,N,Leader])
    end.


%the leader procedure is extended with the argument N, the sequence 
% number of the next message (regular message or view) to be sent.
leader(Id, Master, N, Slaves, Group)->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            io:format("Leader~w got a join request: from Worker{~w,~w} ~n", [Id , Wrk, Peer]),
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop -> 
            ok;
        Unexpected_Message->
            io:format("Leader ID~w got unexpected Message: ~w ~n,current message ~w~n", [Id,Unexpected_Message,N])
    end.


%the election procedure is extended with the same two arguments.
election(Id, Master, N, Last, Slaves, [_|Group])->
    io:format("Electing new leader ( Slave~w ) ~n", [Id]),
    Self = self(),
    case Slaves of
        [Self|Rest] ->
%here we may want something like {view, N+1, Slaves, Group}
            io:format("New Leader is Leader~w ~n", [Id]),
            bcast(Id,Last,Rest),
            bcast(Id, {view,N, Slaves, Group}, Rest),
            Master ! {view, Group},
            %leader(Id, Master, Rest, Group);
            leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
            io:format("Slave~w : New Leader is Leader~w ~n", [Id,Leader]),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.


bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ -> 
            ok
end.