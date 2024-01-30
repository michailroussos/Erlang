-module(storage).
%-compile(export_all).
-export([create/0,add/3,lookup/2,split/3,merge/2]).
-import(key, [between/3,generate/0]).


%-define(Timeout, 1000).
%-define(Stabilize, 1000).

%: create a new store
create()->
    [].

%: add a key-value pair, return the updated store
add(Key, Value, Store)->
    NewStore=lists:keydelete(Key, 1, Store),
    NewStore++[{Key,Value}].

%: return a tuple {Key, Value} or the atom false
lookup(Key, Store)->
    lists:keyfind(Key, 1, Store).


    %: return a tuple {Updated, Rest} where the updated store only
    % contains the key-value pairs requested and the rest are found 
    % in a list of key-value pairs;    
split(From, To, Store)->
    lists:partition(fun({K,_})-> between(K, From, To) end, Store).


%: add a list of key-value pairs to a store
merge(Entries, Store)->
    Entries++Store.