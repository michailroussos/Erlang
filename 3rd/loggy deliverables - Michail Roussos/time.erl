-module(time).
-export([zero/1,inc/2,merge/2,clock/1,safe/3,leq/2,update/3]).

% return an initial Lamport value (could it be 0)
zero(Nodes) ->
    0.

% return the time T incremented by one (you will probably
% ignore the Name, but we will use it later)
inc(Name, T) ->
    T+1.

% merge the two Lamport time stamps (i.e., take the maximum value)
merge(Ti, Tj) ->
    case leq(Ti,Tj) of
        true->
            Tj;
        false->
            Ti
        end.

% true if Ti is less than or equal to Tj
leq(Ti,Tj) ->
    Ti =< Tj.

% return a clock that can keep track of the nodes;
% ****** different implementation for lamport and vector
% we have only one counter per process    
clock(Nodes) ->
%[{clock_name1,time1},{clock_name2,time2},{clock_name2,time3}]
    zero(Nodes).


% return a clock that has been updated
% given that we have received a log message from a node at a given time;
% ****** different implementation for lamport and vector
update(Node, Time, Clock) ->
    inc(Node, merge(Time,Clock)).

% is it safe to log an event that happened at a given time, true or false?
% ****** different implementation for lamport and vector


safe(Name, Time, Clocks) ->
    % Get the Clocks that have greater time
    GreaterTimeEvents = lists:filter(fun({_, X}) -> X >= Time end, Clocks),
    Peerslen = length(Clocks),
    % check if all the nodes have send a message after this specific time
    % so if it safe to print
    case length(GreaterTimeEvents) of
        Peerslen -> % we have received events from all the peers after this time
            true;
        _ -> 
            false % we have not received events from all the peers after this time
    end.