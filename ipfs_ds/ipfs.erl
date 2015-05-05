-module(ipfs).
-export([
    main/1, 
    start/1, 
    get_host_info/1,
    send/3,
    publish_file/5,
    get_file_main/1,
    get_block_main/1,
    download_blocks/5,
    dump_routing_table_main/1,
    store_block_on_disk/3,
    dump_routing_table/2,
    listen/4]).

has_block(Id, Dir) ->
    BlockDir = string:concat(Dir, "/blocks/"),
    File = string:concat(BlockDir, Id),
    filelib:is_file(File).    

fetch_block_from_disk(Id, Dir) ->
    io:format("Fetching block from disk:"),
    io:format(Id),
    io:format("\n"),
    BlockDir = string:concat(Dir, "/blocks/"),
    File = string:concat(BlockDir, Id),
    {ok, Data} = file:read_file(File), 
    Data.

store_block_on_disk(Id, Data, Dir) ->
    io:format("Storing block..."),
    io:format(Id),
    io:format("\n"),
    BlockDir = string:concat(Dir, "/blocks/"),
    TmpName = integer_to_list(erlang:phash2(make_ref())),
    TmpFile = string:concat(BlockDir, TmpName),
    file:write_file(TmpFile, Data),
    File = string:concat(BlockDir, Id),
    file:rename(TmpFile, File).

get_hostname(Host, Id) ->
    NodeName = string:concat("node_", Id),
    string:concat(string:concat(NodeName, "@"), Host).

send(SrcInfo, DestInfo, Message) ->
    {DestHost, DestId, _, _} = DestInfo,
    DestName = get_hostname(DestHost, DestId),
    DestAtom = list_to_atom(DestName),
    {ipfs, DestAtom} ! {Message, self(), SrcInfo}.

download_block(SrcInfo, DestInfo, Id) -> 
    send(SrcInfo, DestInfo, {find_value, Id}),
    receive
        {value, Block} -> Block;
        _ -> io:format("No block found!\n"), nil
    after 
        2000 -> io:format("Timeout!\n"), nil
    end.


download_block_from_neighborhood([], _, BadNodes) ->
    {error, nil, nil, BadNodes};
download_block_from_neighborhood([Node|Rest], Id, BadNodes) ->
	{_, DestInfo} = Node,
    {_, NodeId, _, _} = DestInfo,
    case sets:is_element(NodeId, BadNodes) of
        true -> 
            io:format("~s is blacklisted!\n", [NodeId]),
            download_block_from_neighborhood(Rest, Id, sets:add_element(Node, BadNodes));   
        false ->
            case download_block(nil, DestInfo, Id) of 
                nil -> 
                    io:format("Download failed! Blacklisting and moving on.\n"), 
                    download_block_from_neighborhood(Rest, Id, sets:add_element(NodeId, BadNodes));
                Block -> {ok, Block, DestInfo, BadNodes}
            end
    end.

publish_block_to([], _, _, _) ->
    ok;
publish_block_to([Closest|Rest], BlobId, Data, Source) ->
	{_, Info} = Closest,
	send(Source, Info, {store, BlobId, Data}),
	publish_block_to(Rest, BlobId, Data, Source).

publish_file_blocks([], _, _, _, _) ->
    ok;
publish_file_blocks([BlobId|Rest], ClientDir, Rt, Source, ReplFactor) ->
    io:format("Publishing block: "),
    io:format(BlobId),
    io:format("\n"),
	KClosest = ipfs_dht:get_k_closest_nodes(Rt, BlobId, ReplFactor),
	Data = fetch_block_from_disk(BlobId, ClientDir),
	publish_block_to(KClosest, BlobId, Data, Source),
    publish_file_blocks(Rest, ClientDir, Rt, Source, ReplFactor).

publish_file(MetadataId, ClientDir, Rt, Source, ReplFactor) ->
    Metadata = fetch_block_from_disk(MetadataId, ClientDir),
    {[_, {_, Blocks}]} = jiffy:decode(Metadata),
    BlockIds = [binary:bin_to_list(X) || {[{_, X}, _]} <- Blocks],
    AllIds = [MetadataId | BlockIds],
    publish_file_blocks(AllIds, ClientDir, Rt, Source, ReplFactor).

listen(HostInfo, RoutingTable, ClientDir, ReplFactor) -> 
    receive
        {Message, Callback, SourceInfo} ->
            io:format("Received message: "),
            UpdatedRoutingTable = case SourceInfo of 
                nil -> RoutingTable;
                _ -> ipfs_dht:insert(RoutingTable, SourceInfo)
            end,
            case Message of
                {ping} ->
                    io:format("Ping!\n"),
                    Callback ! {ok, nil};
                {store, Id, Data} ->
                    io:format("Store!\n"),
                    case has_block(Id, ClientDir) of 
                        false -> store_block_on_disk(Id, Data, ClientDir);
                        true -> nil
                    end,
                    Callback ! {ok, nil};
                {get_nodes} ->
                    io:format("Get Nodes!\n"),
                    Response = ipfs_dht:get_all_nodes(UpdatedRoutingTable),
                    Callback ! {ok, Response};
                {find_node, Id} ->
                    io:format("Find node: "),
                    io:format(Id),
                    io:format("\n"),
                    Response = ipfs_dht:get_k_closest_nodes(UpdatedRoutingTable, Id, ReplFactor),
                    Callback ! {ok, Response};
                {publish_file, MetadataId} ->
                    io:format("Publish file: "),
                    io:format(MetadataId),
                    io:format("\n"),
                    publish_file(MetadataId, ClientDir, UpdatedRoutingTable, HostInfo, ReplFactor),
                    Callback ! {ok, nil};
                {find_value, Id} ->
                    io:format("Find value!\n"),
                    case has_block(Id, ClientDir) of 
                        true ->
                            Block = fetch_block_from_disk(Id, ClientDir),
                            Callback ! {value, Block};
                        false ->

                            Neighborhood = ipfs_dht:get_k_closest_nodes(UpdatedRoutingTable, Id, ReplFactor),
                            Callback ! {nodes, Neighborhood}

                    end
            end
        end,
    listen(HostInfo, UpdatedRoutingTable, ClientDir, ReplFactor).

divide([], _, _) -> 
    dict:new();
divide([Cur|Rest], N, I) ->
    Key = I rem N,
    dict:append(Key, Cur, divide(Rest, N, I+1)).

shuffle(List) ->
    [X || {_,X} <- lists:sort([{random:uniform(), N} || N <- List])].

download_blocks([], _, Master, _, _) -> 
    Master ! done;
download_blocks([WorkItem|Rest], Dest, Master, Host, Blacklist) -> 
    {Oid, _} = WorkItem,
    {ipfs, Host} ! {{find_value, Oid}, self(), nil},
    NewBlacklist = receive 
        {value, Block} ->
            io:format("Downloading ~s from ~s\n", [Oid, Host]),
            Path = filename:join([Dest, Oid]),
            file:write_file(Path, Block),
            Blacklist;
        {nodes, Neighborhood} ->
            Result = download_block_from_neighborhood(shuffle(Neighborhood), Oid, Blacklist),
            case Result of
                {ok, Block, From, BadNodes} ->
                    {Nip, Nid, _, _} = From,
                    FromName = get_hostname(Nip, Nid),
                    io:format("Downloading ~s from ~s\n", [Oid, FromName]),
                    Path = filename:join([Dest, Oid]),
                    file:write_file(Path, Block),
                    BadNodes;
                {error, _, _, BadNodes} ->
                    io:format("Failed to downdload!\n"),
                    BadNodes
            end
    end,
    download_blocks(Rest, Dest, Master, Host, NewBlacklist).

start_workers([], _, _, _) -> [];
start_workers([Work | Rest], Dest, Master, Host) ->
    [spawn(
        ipfs, 
        download_blocks, 
        [Work, Dest, Master, Host, sets:new()]) 
    | start_workers(Rest, Dest, Master, Host)].

wait_for_download(0) -> ok;
wait_for_download(N) ->
    receive
        done -> ok
    end,
    wait_for_download(N-1).

get_block_main([Id, Dest, NodeId, NodeIp]) ->
    Host = list_to_atom(get_hostname(NodeIp, NodeId)),
    {ipfs, Host} ! {{find_value, Id}, self(), nil},
    receive 
        {value, Block} ->
            io:format("Downloading ~s from ~s\n", [Id, Host]),
            Path = filename:join([Dest, Id]),
            file:write_file(Path, Block);
        {nodes, Neighborhood} ->
            Result = download_block_from_neighborhood(shuffle(Neighborhood), Id, sets:new()),
            case Result of
                {ok, Block, From, _} ->
                    {Nip, Nid, _, _} = From,
                    FromName = get_hostname(Nip, Nid),
                    io:format("Downloading ~s from ~s\n", [Id, FromName]),
                    Path = filename:join([Dest, Id]),
                    file:write_file(Path, Block);
                {error, _, _, _} ->
                    io:format("Error!\n")
            end
    end.
 
get_file_main([MetadataFile, Dest, NodeId, NodeIp, Nput]) ->
    {ok, Data} = file:read_file(MetadataFile),
    {[_, {_, Links}]} = jiffy:decode(Data),
    {N, _} = string:to_integer(Nput),
    Host = list_to_atom(get_hostname(NodeIp, NodeId)),
    LinkInfo = [extract_link(X) || X <- Links],
    WorkDict = divide(LinkInfo, N, 0),
    WorkList = [dict:fetch(X,WorkDict) || X <- lists:seq(0,N-1)],
    start_workers(WorkList, Dest, self(), Host),
    wait_for_download(N).

extract_link({Data}) ->
    [HashT, SizeT] = Data,
    {_, Hash} = HashT,
    {_, Size} = SizeT,
    {binary:bin_to_list(Hash), Size}.
 
parse_node(Node) ->
    [{_, Host}, {_, Id}, {_, PubKey}, {_, PrivKey}] = Node,
    {binary:bin_to_list(Host), 
     binary:bin_to_list(Id), 
     binary:bin_to_list(PubKey), 
     binary:bin_to_list(PrivKey)}.

get_bootstrap_nodes(BootstrapFile) ->
    {ok, Data} = file:read_file(BootstrapFile),
    {Items} = jiffy:decode(Data),
    [{_, Nodes}] = Items,
    [parse_node(N) || {N} <- Nodes].

get_host_info(Config) ->
    {ok, Data} = file:read_file(Config),
    {Node} = jiffy:decode(Data),
    parse_node(Node).

bootstrap([], _) ->
    ipfs_dht:new();
bootstrap([Node|Rest], HostInfo) ->
    send(HostInfo, Node, {ping}),
    ipfs_dht:insert(bootstrap(Rest, HostInfo), Node).

dump_routing_table_main([NodeId, NodeIp]) ->
    dump_routing_table(NodeId, NodeIp).

print_table([]) ->
    ok;
print_table([Cur|Rest]) ->
    {X,_} = Cur,
    io:format(X),
    io:format("\n"),
    print_table(Rest).
    
dump_routing_table(NodeId, NodeIp) ->
    Host = get_hostname(NodeIp, NodeId),
    HostAtom = list_to_atom(Host),
    {ipfs, HostAtom} ! {{get_nodes}, self(), nil},
    Rt = receive
        {ok, Response} -> Response
    after 
        5000 -> []
    end,
    print_table(Rt).

start(ClientDir) ->
    HostConf = filename:join([ClientDir, ".ipfs"]),
    BootstrapConf = filename:join([ClientDir, ".bootstrap"]),
    HostInfo = get_host_info(HostConf),
    BootstrapNodes = get_bootstrap_nodes(BootstrapConf),
    RoutingTable = bootstrap(BootstrapNodes, HostInfo),
    register(ipfs, spawn(ipfs, listen, [HostInfo, RoutingTable, ClientDir, 3])),
    ok.

main([ClientDir]) ->
    start(ClientDir).
