-module(loggy).
-export([start/1, stop/1]).
-import(vtime, [zero/1,inc/2,merge/2,clock/1,safe/3,leq/2,update/3]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

create_loger_clocks(Nodes)->
    case Nodes of
        [Head|Tail]->
            [{Head,zero(Nodes)}]++create_loger_clocks(Tail);
        []->
            []
        end.
            
stop(Logger) ->
   Logger ! stop.

init(Nodes) ->
    Queue=[],
    Clocks=create_loger_clocks(Nodes),
    loop(Queue,Clocks).

loop(Queue,Clocks) ->
    receive
        {log, From, Time, Msg} ->
            Current=lists:keyfind(From,1,Clocks),
            {CurrentName,CurrentTime}=Current,
            NewClocks=lists:delete(Current,Clocks)++[{CurrentName,merge(CurrentTime,Time)}],
            NewQueue=safe_print_queue(Queue++[{From,Time,Msg}],NewClocks),
            loop(NewQueue,NewClocks);
        stop -> 
            print_queue(Queue)
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

print_queue(Queue)->
    case Queue of
        [Head|Tail]->
            Oldest=get_oldest_message(Head, Tail),
            {OldestFrom,OldestTime,OldestMsg}=Oldest,
            log(OldestFrom,OldestTime,OldestMsg),
            NewQueue=lists:delete(Oldest,Queue),
            print_queue(NewQueue);
        []->
            true
        end.

%similar to the normal print but it checks whether the messeges are safe to print
% and returns the remaining Queue
safe_print_queue(Queue,Clocks)->
    case Queue of
        [Head|Tail]->
            Oldest=get_oldest_message(Head, Tail),
            {OldestFrom,OldestTime,OldestMsg}=Oldest,
            case safe(OldestFrom,OldestTime,Clocks) of
                true->
                    log(OldestFrom,OldestTime,OldestMsg),
                    NewQueue=lists:delete(Oldest,Queue),
                    safe_print_queue(NewQueue,Clocks);
                false->
                    Queue
                end;
        []->
            Queue
        end.


get_oldest_message(Old,Queue)->
    case Queue of
        [Head|Tail]->
            {_,OldTime,_}=Old,
            {_,HeadTime,_}=Head,
            case leq(OldTime,HeadTime) of
                true->
                    NewOld=Old;
                _->
                    NewOld=Head
                end,
            get_oldest_message(NewOld,Tail);
        []->
            Old
        end.