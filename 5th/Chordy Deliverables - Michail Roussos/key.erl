-module(key).
-export([between/3,generate/0]).

between(Key,From,To) ->
    if
        To > From ->
            if 
                (Key > From) and (Key =< To) ->
                    true;
                true -> 
                    false
            end;
        To == From ->
            true;
        true ->
            if 
                (Key > From) and (Key > To) ->
                    true;
                (Key < From) and (Key =< To) -> 
                    true;
                true ->
                    false
            end
    end.

generate()->
    % will return a random number from 1 to 1000000000
    random:uniform(1000000000).

