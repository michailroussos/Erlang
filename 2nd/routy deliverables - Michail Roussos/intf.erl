-module(intf).
-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).

%return an empty set of interfaces.
new()->
    [].

% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf)->
    % here adding all the nodes will create multiples
    %Intf++[{Name,Ref,Pid}].

    case lists:keyfind(Name,1,Intf) of
        false->
            Intf++[{Name,Ref,Pid}];
        {Name,_,_}->
            Intf
        end.

%remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf)->
    lists:keydelete(Name, 1, Intf).

% find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf)->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_,_,Pid}->
            {ok, Pid}
        end.

% find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf)->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_,Ref,_}->
            {ok, Ref}
        end.

% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf)->
    case lists:keyfind(Ref, 2, Intf) of
        false ->
            notfound;
        {Name,_,_}->
            {ok, Name}
        end.

% return a list with all names.
list(Intf)->
    list_rec(Intf,[]).


list_rec(Intf,List)->
    case Intf of
        [{Name,_,_}|Tail]->
            NewList=List++[Name],
            list_rec(Tail,NewList);
        _->
            List
        end.

% send the message to all interface processes.
broadcast(Message, Intf)->
    broadcast_rec(Message, Intf).

broadcast_rec(Message,Intf)->
    case Intf of
        [{_,_,Pid}|Tail]->
            Pid ! Message,
            broadcast_rec(Message, Tail);
        _->
            %no action here
            true
        end.

