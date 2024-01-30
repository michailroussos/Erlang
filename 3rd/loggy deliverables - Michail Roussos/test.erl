-module(test).
-export([run/2]).
%report on your initial observations
run(Sleep, Jitter) ->
    Log = loggy:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    worker:peers(A, [B,C,D] ,[paul, ringo, george]),
    worker:peers(B, [A,C,D] ,[john, ringo, george]),
    worker:peers(C, [A,B,D] ,[john, paul, george]),
    worker:peers(D, [A,B,C] ,[john, paul, ringo]),
    timer:sleep(5000),
    loggy:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).