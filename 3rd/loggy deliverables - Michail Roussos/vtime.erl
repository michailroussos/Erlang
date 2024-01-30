-module(vtime).
-export([zero/1,inc/2,merge/2,clock/1,safe/3,leq/2,update/3]).

% return an initial Lamport value (could it be 0)
zero(Nodes) ->
    %maybe we need an extra argument here for the name of the node
    % and we could return [{Name,0}]?
    % or even maybe recieve the peers as well and do clocks(Peers++[Name])
    %[{clock_name1,time1},{clock_name2,time2},{clock_name2,time3}]
    lists:foldl(fun(X, Clock_List) -> [{X,0}] ++ Clock_List end, [], Nodes).


% return the time T incremented by one (you will probably
% ignore the Name, but we will use it later)
inc(Name, T) ->
    % maybe foldl and check if each node is the one with the correct name
    % if so increase time, else leave the same?
    %io:format("log: ~w~n", [T]),
    CurrentClock= lists:keyfind(Name,1,T),
    {Node,Time}=CurrentClock,
    lists:delete(CurrentClock,T)++[{Node,Time+1}].
            
% merge the two vector time stamps (i.e., take the maximum value)
merge(Ti, Tj) ->
    case Ti of
        [Head|Tail]->
            {HeadName,HeadTime}=Head,
            TjNode=lists:keyfind(HeadName,1,Tj),
            %io:format("log head : ~w~n", [Head]),
            %io:format("log list: ~w~n", [Tj]),
            {_,TjTime}=TjNode,
            NewTj=lists:delete(TjNode,Tj),
            case TjTime>HeadTime of
                true->
                    [{HeadName,TjTime}]++merge(Tail,NewTj);
                false->
                    [{HeadName,HeadTime}]++merge(Tail,NewTj)
            end;
        []->
            []
    end.


% true if Ti is less than or equal to Tj


leq([], _) ->
    true;
leq([{Name, Ti}|Rest],Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            case Ti =< Tj of
                true->
                    leq(Rest,Time);
                false->
                    false
            end; 
        false ->
            false
    end.

% return a clock that can keep track of the nodes;
% ****** different implementation for lamport and vector
% we keep track of the counters of all the processes    
clock(Nodes) ->
    zero(Nodes).

% return a clock that has been updated
% given that we have received a log message from a node at a given time;
% ****** different implementation for lamport and vector
update(Node, Time, Clock) ->
    inc(Node, merge(Time,Clock)).


% is it safe to log an event that happened at a given time, true or false?
% ****** different implementation for lamport and vector

safe(Name, Time, Clocks) ->
    %iterate throught the list if clocks
    case Clocks of
        [Head|Tail]->
            {_,ClockTime}=Head,
            %check if the time is less or equal than the clock
            % if so we have received messages after this event so it is safe to prin
            case leq(Time,ClockTime) of
                true->
                    %check the rest of the list
                    safe(Name, Time, Tail);
                false->
                    %if not we know it is not safe
                    false
                end;
        % if no issues were met, we know it is safe
        []->
            true
    end.

