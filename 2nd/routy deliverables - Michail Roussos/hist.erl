-module(hist).
-export([new/1,update/3]).

%Return a new history, where messages from Name will always be seen as old.
new(Name) ->
    [{Name,inf}].

%Check if message number N from the Node is old or new. If it is old, then return old, but if it is new, return {new, Updated} where Updated is the updated history.
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        false ->
            {new,History++[{Node,N}]};
        {Node,ExistingN} when ExistingN < N->
            NewHistory=lists:keydelete(Node, 1, History),
            {ok, NewHistory++[{Node,N}]};
        _->
            old
        end.