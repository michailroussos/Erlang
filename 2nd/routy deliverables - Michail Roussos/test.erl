-module(test).
-export([start/1,broadcast/0, update/0]).

start(Name) ->
    routy:start(r1, stockholm),
    routy:start(r2, uppsala),
    routy:start(r3, gothenburg),
    routy:start(r4, nybro),
    routy:start(r5, kristianstad),

    r1 ! {add, uppsala, {r2,Name}},
    r1 ! {add, gothenburg, {r3,Name}},
    r1 ! {add, nybro, {r4,Name}},
    r2 ! {add, kristianstad, {r5,Name}},
    r2 ! {add, gothenburg, {r3,Name}},
    r4 ! {add, stockholm, {r1,Name}},
    r5 ! {add, gothenburg, {r3,Name}},
    r5 ! {add, gothenburg, {r3,Name}},
    timer:sleep(4000),
    broadcast(),
    timer:sleep(4000),
    update(),
    routy:print_status(r1),
    routy:print_status(r2),
    routy:print_status(r3),
    ok.

broadcast() ->
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast.

update() ->
    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update.