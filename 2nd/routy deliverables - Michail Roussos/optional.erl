-module(optional).
-export([start/0]).
% 192.168.220.33
% 192.168.220.207
start() ->
    routy:start(stockholm, stockholm),
    routy:start(uppsala, uppsala),
    routy:start(gothenburg, gothenburg),
    routy:start(kristianstad, kristianstad),

    stockholm ! {add, uppsala, {uppsala,'sweden@192.168.220.33'}},
    stockholm ! {add, gothenburg, {gothenburg,'sweden@192.168.220.33'}},
    uppsala ! {add, gothenburg, {gothenburg,'sweden@192.168.220.33'}},
    gothenburg ! {add, kristianstad, {kristianstad,'sweden@192.168.220.33'}},
    kristianstad ! {add, stockholm, {stockholm,'sweden@192.168.220.33'}},
    %stockholm ! {add, bergen, {bergen,'norway@192.168.220.207'}}.
    % stockholm ! update.
    % stockholm!{send,bergen,"hello"}.
    % stockholm!{send,paris,"hello1"}.
    timer:sleep(1000),
    broadcast(),
    timer:sleep(1000),
    update(),
    ok.

    broadcast() ->
        stockholm ! broadcast,
        uppsala ! broadcast,
        gothenburg ! broadcast,
        kristianstad ! broadcast.
    
    update() ->
        stockholm ! update,
        uppsala ! update,
        gothenburg ! update,
        kristianstad ! update.