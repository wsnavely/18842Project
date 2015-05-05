-module(ipfs_dht).
-export([
    new/0,
    insert/2, 
    get_all_nodes/1,
    get_k_closest_nodes/3]).

new() ->
    dict:new().

insert(Rt, Info) ->
    {_, Id, _, _} = Info,
    dict:store(Id, Info, Rt).

get_k_closest_nodes(Rt, Id, K) ->
    List = dict:to_list(Rt),
    Cmp = fun({K1,_}, {K2,_}) -> dist(Id, K1) =< dist(Id, K2) end,
    Sorted = lists:sort(Cmp, List),
    lists:sublist(Sorted, K).

get_all_nodes(Rt) ->
    dict:to_list(Rt).

id_to_int(Id) -> 
    erlang:list_to_integer(Id, 16).

dist(X,Y) ->
    id_to_int(X) bxor id_to_int(Y).
