-module(ipfs_helper).
-export([publish_block_helper/1, publish_file_helper/1]).

publish_file_helper([Dest, Mid]) ->
	io:fwrite("[Helper]: Publishing file."),
	DestAtom = list_to_atom(Dest),
	{ipfs, DestAtom} ! {{publish_file, Mid}, self(), nil},
    receive
        X->X
    end.

publish_block_helper([Dest, BlobId]) ->
	io:fwrite("[Helper]: Publishing block."),
	publish_block_helper_main(Dest, BlobId).

publish_block_helper_main(Dest, BlobId) ->
	DestAtom = list_to_atom(Dest),
	{ipfs, DestAtom} ! {{publish, BlobId}, self(), nil}.

