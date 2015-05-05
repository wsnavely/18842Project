%% This module is for routing table operations
%% Such as new a routing table when a node is started,
%% Insertion and deletion of nodes

-module(dht_rt).

% This is for routing table creation, insertion of nodes
% as well as deletion of nodes
-export([new/1, insert/2, delete/2]).

-define(K, 8).
-define(in_range(Dist, Min, Max), ((Dist >= Min) andalso (Dist < Max))).

-record(routing_table,
    { self :: dht:node_id(),
	  table :: {dht:node_id(), dht:node_id(), [dht:node_id()]}
	}).

-type t() :: #routing_table{}.

%%
%% Create a new bucket
%%
-spec new(dht:node_id()) -> t().
new(Self) when is_integer(Self), Self >= 0 ->
	MaxID = 1 bsl 160,
	#routing_table {
		self = Self,
		table = [{0, MaxID, []}]
	}.

%%
%% Insert a new node into a bucket list
%%
-spec insert(dht:node_t(), t()) -> t().
insert({ID, _, _} = Node, #routing_table { self = Self, table = Buckets} = Tbl) ->
    {Rest, Acc} = insert_(dht_metric:d(Self, ID), Self, Node, ?K, Buckets, []),
    Tbl#routing_table { table = lists:reverse(Acc) ++ Rest}.

%% The recursive runner for insertion
insert_(0, _Self, _Node, _K, Buckets, Acc) ->
    {Buckets, Acc};
insert_(1, _Self, Node, _K, [{Min=0, Max=1, Members}], Acc) ->
    NewMembers = ordsets:add_element(Node, Members),
    {[{Min, Max, NewMembers}], Acc};
insert_(Dist, Self, Node, K, [{Min, Max, Members} | Next], Acc)
    when ?in_range(Dist, Min, Max) ->
    %% We analyze the numbers of members and either insert or split the bucket
    case length(Members) of
        L when L < K ->
            {[{Min, Max, ordsets:add_element(Node, Members)} | Next], Acc};
        L when L == K, Next /= [] ->
            {[{Min, Max, Members} | Next], Acc};
        L when L == K ->
            Splitted = insert_split_bucket({Min, Max, Members}, Self),
            insert_(Dist, Self, Node, K, Splitted, Acc)
            end;
insert_(Dist, Self, Node, K, [H|T], Acc) ->
    insert_(Dist, Self, Node, K, T, [H|Acc]).

%% Split buckets into two, with range of original bucket into 2 separated ones
insert_split_bucket({Min, Max, Members}, Self) ->
    Diff = Max - Min,
    Half = Max - (Diff div 2),
    {Lower, Upper} = split_partition(Members, Self, Min, Half),
    [{Min, Half, Lower}, {Half, Max, Upper}].

%% Split the bucket which the node is in into half
%% Return the two splitted lists
split_partition(Members, Self, Min, Half) ->
    {Lower, Upper} =
        lists:foldl(fun ({MID, _, _} = N, {Ls, Us}) ->
            case ?in_range(dht_metric:d(MID, Self), Min, Half) of
                true -> {[N | Ls ], Us};
                false -> {Ls, [N | Us]}
                end
            end,
            {[], []},
            Members),
    {lists:reverse(Lower), lists:reverse(Upper)}.

%%
%% Delete a node from a bucket list
%%
-spec delete(dht:node_t(), t()) -> t().
delete({ID, _, _} = Node, #routing_table { self = Self, table = RoutingTable} = Tbl) ->
    {Rest, Acc} = delete_(dht_metric:d(ID, Self), Node, RoutingTable, []),
    Tbl#routing_table { table = lists:reverse(Acc) ++ Rest }.

delete_(_, _, [], Acc) -> {[], Acc};
delete_(Dist, Node, [{Min, Max, Members}|T], Acc) when ?in_range(Dist, Min, Max) ->
    NewMembers = ordsets:del_element(Node, Members),
    {[{Min, Max, NewMembers}|T], Acc};
delete_(Dist, Node, [H|T], Acc) ->
    delete_(Dist, Node, T, [H|Acc]).


