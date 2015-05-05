-module(ipfs_demo).
-export([
    main/1, 
    start/1, 
    listen/2, 
    closest_k/3, 
    get_file/4, 
    get_file_main/1, 
    get_host_info/1, 
    download_blocks/4, 
    fetch_block_from_disk/2]).

id_to_int(Id) -> 
    erlang:list_to_integer(Id, 16).

dist(X,Y) ->
    id_to_int(X) bxor id_to_int(Y).

has_block(Id, Dir) ->
    BlockDir = string:concat(Dir, "/blocks/"),
    File = string:concat(BlockDir, Id),
    file:is_file(File).    

fetch_block_from_disk(Id, Dir) ->
    BlockDir = string:concat(Dir, "/blocks/"),
    File = string:concat(BlockDir, Id),
    {ok, Data} = file:read_file(File), 
    Data.

store_block_on_disk(Id, Data, Dir) ->
    BlockDir = string:concat(Dir, "/blocks/"),
    TmpName = erlang:phash2(make_ref()),
    TmpFile = string:concat(BlockDir, TmpName),
    file:write_file(TmpFile, Data),
    File = string:concat(BlockDir, Id),
    file:rename(TmpFile, File).

closest_k(Oid, Hosts, K) ->
    Cmp = fun({_,A}, {_,B}) -> dist(Oid, A) =< dist(Oid, B) end,
    Sorted = lists:sort(Cmp, Hosts),
    lists:sublist(Sorted, K).
    
listen(HostInfo, ClientDir) -> 
    receive
        {request_block, Sender, Id} -> 
            Block = fetch_block_from_disk(Id, ClientDir),
            Sender ! { send_block, Block };
        {ping, Sender} ->
            Sender ! { response, ok };
        {store, Id, Data, Sender} ->
            store_block_on_disk(Id, Data, ClientDir),
            Sender ! { response, ok };
        {find_node, Id, Sender} ->
            Sender ! { response, ok };
        {find_value, Id, Sender} ->
            Sender ! { response, ok }
    end,
    listen(HostInfo, ClientDir).

extract_info({Data}) ->
    [_, HostT, IdT] = Data,
    {_, Host} = HostT,
    {_, Id} = IdT,
    {binary:bin_to_list(Host), binary:bin_to_list(Id)}.

extract_link({Data}) ->
    [HashT, SizeT] = Data,
    {_, Hash} = HashT,
    {_, Size} = SizeT,
    {binary:bin_to_list(Hash), Size}.
    
divide([], _, _) -> 
    dict:new();
divide([Cur|Rest], N, I) ->
    Key = I rem N,
    dict:append(Key, Cur, divide(Rest, N, I+1)).

pick(List) ->
    Index = random:uniform(length(List)),
    lists:nth(Index,List).

get_host_name(Host, Id) ->
    NodeName = string:concat("node_", Id),
    string:concat(string:concat(NodeName, "@"), Host).

download_blocks([], _, Master, _) -> 
    Master ! done;
download_blocks([WorkItem|Rest], Dest, Master, Hosts) -> 
    {Oid, _} = WorkItem,
    Neighborhood = closest_k(Oid, Hosts, 2),
    Neighbor = pick(Neighborhood),
    {Host, Hid} = Neighbor,
    HostName = list_to_atom(get_host_name(Host, Hid)),
    io:format("~s,~s~n", [Oid, Hid]),
    {ipfs, HostName} ! {request_block, self(), Oid},
    receive 
        {send_block, Block} ->
            Path = string:concat(Dest, Oid),
            file:write_file(Path, Block),
            ok
    end,
    download_blocks(Rest, Dest, Master, Hosts).

start_workers([], _, _, _) -> [];
start_workers([Work | Rest], Dest, Master, Hosts) ->
    [spawn(
        ipfs_demo, 
        download_blocks, 
        [Work, Dest, Master, Hosts]) 
    | start_workers(Rest, Dest, Master, Hosts)].

wait_for_download(0) -> ok;
wait_for_download(N) ->
    receive
        done -> ok
    end,
    wait_for_download(N-1).

get_file(BlockList, Dest, Hosts, N) ->
    {ok, Data} = file:read_file(BlockList),
    {[_, LinksT]} = jiffy:decode(Data),
    {_, Links} = LinksT,
    LinkInfo = [extract_link(X) || X <- Links],
    WorkDict = divide(LinkInfo, N, 0),
    WorkList = [dict:fetch(X,WorkDict) || X <- lists:seq(0,N-1)],
    start_workers(WorkList, Dest, self(), Hosts),
    wait_for_download(N).

get_file_main([BlockList, Dest, Config, Nput]) ->
    {ok, Data} = file:read_file(BlockList),
    {[_, LinksT]} = jiffy:decode(Data),
    {_, Links} = LinksT,
    {N, _} = string:to_integer(Nput),
    HostInfo = get_host_info(Config),
    LinkInfo = [extract_link(X) || X <- Links],
    WorkDict = divide(LinkInfo, N, 0),
    WorkList = [dict:fetch(X,WorkDict) || X <- lists:seq(0,N-1)],
    start_workers(WorkList, Dest, self(), HostInfo),
    wait_for_download(N).

start(ClientDir) -> 
    Config = filename:join([ClientDir, ".ipfs"]), 
    HostInfo = get_host_info(Config),
    {HostName, LocalId, _, _} = HostInfo,
    ProcessName = list_to_atom(string:concat("node_", LocalId)),
    register(ipfs, spawn(ipfs_demo, listen, [HostInfo, ClientDir])),
    {HostInfo, ProcessName}.

main([ClientDir]) ->
    start(ClientDir).

get_host_info(Config) ->
    {ok, Data} = file:read_file(Config),
    {Node} = jiffy:decode(Data),
    [{_, Host}, {_, Id}, {_, PubKey}, {_, PrivKey}] = Node,
    {binary:bin_to_list(Host), binary:bin_to_list(Id), binary:bin_to_list(PubKey), binary:bin_to_list(PrivKey)}.
    
%    [extract_info(X) || X <- Nodes].
