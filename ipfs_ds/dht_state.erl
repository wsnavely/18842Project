-module(dht_state).
-behavior(gen_server).

-export([
	node_id/0
]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
]).

－record(state, {
	node_id :: dht:node_id(),
	routing_table :: dht_routing_table:t(),
}).

%% @doc Return the node id.
%% Node ids now are now generated in a random manner.
-spec node_id() -> dht:node_id().
node_id() ->
	gen_server:call(?MODULE, node_id).


handle_call(node_id, _From, #state{ node_id = Self } = State) ->
    {reply, Self, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_, State) ->
	ok.

%% @private
code_change(_, State, _) ->
    {ok, State}.
