-module(dht_metric).

-export([make_id/0, dist/2]).
-export([neighborhood/3]).

%% @doc Right now just make a random number as node id, should be optimized here for the hash value
%% @end
-spec make_id() ->  dht:id().
make_id() ->
%%: TODO: make the node id with a hash function instead of random number
	<<ID:160>> = crypto:rand_bytes(20),
	ID.

%% @doc dist/2 calculates the distance between two random IDs
%% @end
-spec dist(dht:id(), dht:id()) ->  dht:id().
dist(ID1, ID2) -> ID1 bxor ID2.

%% @doc neighborhood/3 finds known nodes close to an ID
%% neighborhood(ID, Nodes, Limit) searches for Limit nodes in the neighborhood of ID. Nodes is the list of known nodes.
%% @return the list of nodes in after filtering by neighborhood
%% @end
-spec neighborhood(ID, Nodes, Limit) -> [dht:node_t()]
  when
    ID :: dht:id(),
    Nodes :: [dht:node_t()],
    Limit :: non_neg_integer().

neighborhood(ID, Nodes, Limit) ->
	Distances = [{dist(ID, NID), N} || {NID, _, _} = N <- Nodes],
	Eligible = case lists:sort(Distances) of
	    Sorted when length(Sorted) =< Limit -> Sorted;
	    Sorted when length(Sorted) > Limit ->
	        {H, _T} = lists:split(Limit, Sorted),
	        H
	end,
	[S || {_, S} <- Eligible].
