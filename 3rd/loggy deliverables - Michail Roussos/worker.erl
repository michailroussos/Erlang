-module(worker).
-export([start/5, stop/1, peers/3]).
-import(vtime, [zero/1,inc/2,merge/2,leq/2,safe/3,clock/1,update/3]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed,Seed,Seed),
    receive
        {peers, Peers, PeersNames} ->
            %I added an attribute here for the new Clock
            loop(Name, Log, Peers, Sleep, Jitter,zero(PeersNames++[Name]));
        stop -> 
            ok
        end.

peers(Wrk, Peers , PeersNames) ->
    Wrk ! {peers, Peers, PeersNames}.

%I added an attribute here for the new Clock    
loop(Name, Log, Peers, Sleep, Jitter, Clock)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            %we update the time here
            NewTime= update(Name, Time, Clock),
            % we send the new time
            Log ! {log, Name, NewTime, {received, Msg}},
            % we re initiate the loop with the new time
            loop(Name, Log, Peers, Sleep, Jitter, NewTime);
        stop -> 
            ok;
        Error ->
            % here instead of Clock it had the value time , so I though
            % it would be good to specify when the error occured
            Log ! {log, Name, Clock, {error, Error}}
    after Wait ->
            Selected = select(Peers),
            %here we update the time
            Time = inc(Name,Clock),
            Message = {hello, random:uniform(1000)},
            Selected ! {msg, Time, Message},
            jitter(Jitter),
            Log ! {log, Name, Time, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;

jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).