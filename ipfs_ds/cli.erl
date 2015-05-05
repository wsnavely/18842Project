%%
%% IPFS CLI module.
%%
-module(cli).
-export([cli/0]).

cli() ->
	io:fwrite("Welcome to IPFS."),
	cli_loop().

cli_loop() ->
	Line = io:get_line("ipfs > "),
	Tokens = string:tokens(Line, " "),
	handle_cmd(lists:reverse(Tokens)),
	io:fwrite("\n"),
	cli_loop().

%% ls UNIX command
handle_cmd_ls(CmdArgs) ->
	io:fwrite("This is ls command.").

%% cat UNIX command
handle_cmd_cat(CmdArgs) ->
	FileName = lists:last(CmdArgs),
	io:fwrite("Chunking...\n"),
	blocks:chunk_file(FileName).

%% help command: Lists all the available commands
handle_cmd_help(CmdArgs) ->
	io:fwrite("This is help command.").

handle_cmd(Cmd) ->
	CmdType = lists:last(Cmd),
	case CmdType of
		%%
		%% Be careful here. When a _command_ does not have any arguments, it ends
		%% with a '\n'. See the command 'help' for example. But, if it has arguments,
		%% it does not end with a '\n'. There might be a way to simplify this and
		%% get rid of '\n's! But, till then, make sure that you get it right!
		%%
		_ when CmdType == "ls\n"   -> handle_cmd_ls(lists:droplast(Cmd));
		_ when CmdType == "cat"  -> handle_cmd_cat(lists:droplast(Cmd));
		_ when CmdType == "help\n" -> handle_cmd_help(lists:droplast(Cmd));
		_ -> io:fwrite("Unknown command.")
	end.
