%%
%% IPFS Blocks module
%% - Convert files to blocks
%%
-module(blocks).
-export([chunk_file/1]).
-define(BLK_SIZ, 1024).

%% List printing APIs
print_list([]) -> io:fwrite("End\n");
print_list(ThisList) ->
	Str = lists:last(ThisList),
	io:fwrite("~B\n", [Str]),
	print_list(lists:droplast(ThisList)).

%%
%% chunk_file:
%% Returns a list of chunk hashes.
%% TODO: Also, should return the list of actual chunk data.
%%
chunk_file(FileName) ->
	io:fwrite("Chunking file.\n"),
	%% Following is a function that calculates hashes of chunks:
    F = fun(Bin, Int, List) ->
				io:fwrite("-----New Chunk----\n"),
												% io:fwrite("List: "),
												% print_list(List),
				%% TODO: Don't use MD5. Must use SHA256 or something.
				DigestBin = crypto:hash(md5, Bin),
				DigestList = binary_to_list(DigestBin),
				lists:foreach(fun(Elem)-> io:fwrite("~.16B:", [Elem]) end, DigestList),
				io:fwrite("\n"),
				NewList = lists:append(List, [Int]),
				{more, count_x(Bin) + Int, NewList}
        end,

    with_file(FileName, F, 0, []).


%% This function will go away eventually.
count_x(Bin) ->
    DataList = binary_to_list(Bin),
	lists:flatlength(DataList).

with_file(File, Fun, Initial, List) ->
    case file:open(File, [read, raw, binary]) of
        {ok, Fd} ->
            Res = feed(Fd, file:read(Fd, ?BLK_SIZ), Fun, Initial, List),
            file:close(Fd),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.

feed(Fd, {ok, Bin}, Fun, Farg, List) ->
    case Fun(Bin, Farg, List) of
        {done, Res, List} ->
            {Res, List};
        {more, Ack, RetList} ->
            feed(Fd, file:read(Fd, ?BLK_SIZ), Fun, Ack, RetList)
    end;

feed(Fd, eof, Fun, Ack, List) ->
    Ack;

feed(_Fd, {error, Reason}, _Fun, _Ack, List) ->
    {error, Reason}.
