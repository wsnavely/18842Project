-module(dht).

-type id() :: non_neg_integer().
-type tag() :: binary().
-type token() :: binary().

-type node_id() :: non_neg_integer().
-type node_t() :: {node_id(), inet:ip_address(), inet:port_number()}.
-type peer_info() :: {inet:ip_address(), inet:port_number()}.

-export_type([id/0, tag/0, token/0]).
-export_type([node_id/0, node_t/0, peer_info/0]).
