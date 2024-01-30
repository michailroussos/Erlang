-module(dijkstra).
-export([entry/2,replace/4,update/4,iterate/3,table/2,route/2]).

%returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted)->
    case lists:keyfind(Node, 1, Sorted) of
        false ->
            0;
        {_,Length,_}->
            Length
        end.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should, of course, be sorted.
replace(Node, N, Gateway, Sorted)->
    lists:sort(fun compare_nodes/2, replace_rec(Node, N, Gateway, Sorted)).

% if the list is empty, return an empty list
replace_rec(_, _, _, []) ->
    [];
% if we find the node that we want to replace, we create the replacement node,
% attach the rest of the list behind it and return
replace_rec(Node, N, Gateway, [{Node, _, _} | Rest]) ->
    [{Node, N, Gateway} | Rest];
% if the current node is not the one we want, continue parsing the list
replace_rec(Node, N, Gateway, [Entry | Rest]) ->
    [Entry | replace_rec(Node, N, Gateway, Rest)].

%update the list Sorted given the information that Node can be reached in N hops using Gateway. If no entry is found, then no new entry is added. Only if we have a better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted)->
    case lists:keyfind(Node, 1, Sorted) of
        % if no entry is found, do nothing.
        false ->
%           %lists:keysort(2,Sorted);
            Sorted;
        % if a path with greater lenght is found, replace it
        {Node, Length, _} when N < Length ->
            replace(Node, N, Gateway, Sorted);
        % if no better path found, keep the existing entry.
        _ ->
%           %lists:keysort(2,Sorted)
            Sorted
    end.

%this is used to compare nodes, in order to use this function for sorting
compare_nodes(Node1,Node2)->
    {_,X,_}=Node1,
    {_,Y,_}=Node2,
    X < Y.

% construct a table given a sorted list of nodes, a map, and a table constructed so far.
iterate(Sorted, Map, Table)->
    iterate_rec(Sorted,Map,Table).


iterate_rec(Sorted, Map, Table)->
    case Sorted of
        %If there are no more entries in the sorted list,
        % then we are done, and the given routing table is complete
        []->
            Table;
        %If the first entry is a dummy entry with an infinite path to a city,
        % we know that the rest of the sorted list is also of infinite length,
        % and the given routing table is complete.
        [{_,inf,_}|_]->
            Table;

        %Otherwise, take the first entry in the sorted list, 
        % find the nodes in the map reachable from this entry, 
        % and for each of these nodes, update the Sorted list.
        % The entry that you took from the sorted list is added to the routing table.
        [Head|Tail]->
            {Destination,_,Gateway}=Head,
            NewTable=Table++[{Destination,Gateway}]++add_paths_to_table(Gateway,Map),
            iterate_rec(Tail, Map, NewTable)
        %    iterate_rec(Tail, Map, Table)
        end.

%this function will create all the paths needed for a Gateway given a specific Map
add_paths_to_table(Gateway,Map)->
    %we invoke the recursive function with the the gateway and 
    % the list of the reachable destinations from there
    add_paths_to_table_rec(Gateway,map:reachable(Gateway, Map),[]).

%we loop through the destinations and add a pair for each!
add_paths_to_table_rec(Gateway,Destinations,Paths)->
    case Destinations of
        []->
            Paths;
        [Head|Tail]->
            NewPaths=Paths++[{Head,Gateway}],
            add_paths_to_table_rec(Gateway,Tail,NewPaths)
        end.


%{berlin, 2, paris}
%{destination, distance, gateway}

% construct a routing table given the gateways and a map.
table(Gateways, Map)->
    Table=table_gateways_rec(Gateways,Map,[]),
    table_map_rec(Map,Table).

%this will add all the direct nodes in the Table
table_gateways_rec(Gateways,Map,Table)->
    case Gateways of
        []->
            Table;
        [Head|Tail]->
            NewTable=[{Head,Head}]++Table,
            table_gateways_rec(Tail,Map,NewTable)
        end.

table_map_rec(Map,Table)->
    case Map of
        []->
            Table;
        [Head|Tail]->
            {Start,Destinations}=Head,
            %NewTable=Table++table_map_node_rec(Start,Destinations,Table),
            NewTable=table_map_node_rec(Start,Destinations,Table),
            table_map_rec(Tail,NewTable)
        end.
        
table_map_node_rec(Start,Destinations,Table)->
    case Destinations of
        []->
            Table;
        [Head|Tail]->
            %check if there is a path to this destination already
            case lists:keyfind(Head, 1, Table) of
                %if yes , just skip this destination
                {Head,_}->
                    table_map_node_rec(Start,Tail,Table);
                %if no then add this to the table and go to the next
                false->
                    NewTable=Table++[{Head,Start}],
                    table_map_node_rec(Start,Tail,NewTable)
                end
        end.


%  search the routing table and return the gateway suitable to route messages to a node. If a gateway is found, we should return {ok, Gateway}; otherwise, we return notfound.
route(Node, Table)->
    case lists:keyfind(Node, 1, Table) of
        % if no entry is found, return notfound.
        false ->
            notfound;
        % if we found a record we can return the Gateway
        {Node, Gateway} ->
            {ok,Gateway}
    end.