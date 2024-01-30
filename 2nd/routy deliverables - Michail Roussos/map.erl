-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

% create a new map
new()->
    [].

% set the connections of the Node to be the ones in the list Links
update(Node, Links, Map)->
    %we delete old entries
    NewMap=lists:keydelete(Node, 1, Map),
    %we add the new connecctions
    Result= NewMap++[{Node,Links}],
    Result.

% returns the nodes that are directly reachable from the node
reachable(Node, Map)->
    %we look for the node and if we find it we return its connections
    case lists:keyfind(Node, 1, Map) of
        false ->
            [];
        {_,Links}->
            Links
        end.

% returns a list of all the nodes in the map
all_nodes(Map)->
    Result = all_nodes_rec_source(Map,[]),
    FinalResult = all_nodes_rec_destination(Map,Result),
    FinalResult.

%this addes all the sources to a list and returns it
all_nodes_rec_source(Map, Result)->
    case Map of
        [] ->
            Result;
        [{Node,_}|Tail] ->
            case lists:keyfind(Node, 1, Result) of
                false ->
                    NewResult=Result++[Node],
                    all_nodes_rec_source(Tail, NewResult);
                _->
                    all_nodes_rec_source(Tail, Result)
            end
    end.

%here we add all the destinations in a list and return it
all_nodes_rec_destination(Map,Result)->
    case Map of
        [] ->
            Result;
        [{_,Destination_List}|Tail] ->
            NewResult=all_nodes_rec_destination_list(Destination_List,Result),
            all_nodes_rec_destination(Tail,NewResult)
    end.

all_nodes_rec_destination_list(Destination_List,Result)->
    case Destination_List of
        [] ->
            Result;
        [Node|Tail] ->
            case lists:keyfind(Node, 1, Result) of
                %if the node does not exist in the link then we add it
                false ->
                    NewResult=Result++[Node],
                    all_nodes_rec_destination_list(Tail, NewResult);
                %if the node exists in the list then we move on.
                _->
                    all_nodes_rec_destination_list(Tail, Result)
            end
    end.