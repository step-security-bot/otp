%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    file_io.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-04-13

-module(fileio).
-export([benchmark_arguments/0, benchmark_unit/0]).

-export( [
%	advise,
%	pwrite,
%	datasync
%	sync
%	readline,
%	read_file,
%	delete,
	list_dir/3,
	read_file_info/3
    ]).

% {open,3},
% {open,2},
% {open,1},
% {open_int,4},
% {close,1},
% {advise,4},
% {write,2},
% {pwrite,2},
% {pwrite,3},
% {datasync,1},
% {sync,1},
% {read_line,1},
% {read,2},
% {pread,2},
% {pread,3},
% {position,2},
% {truncate,1},
% {copy,3},
% {ipread_s32bu_p32bu,3},
% {read_file,1},
% {read_file,2},
% {write_file,2},
% {start,0},
% {stop,1},
% {get_cwd,0},
% {get_cwd,1},
% {get_cwd,2},
% {set_cwd,1},
% {set_cwd,2},
% {delete,1},
% {delete,2},
% {rename,2},
% {rename,3},
% {make_dir,1},
% {make_dir,2},
% {del_dir,1},
% {del_dir,2},
% {read_file_info,1},
% {read_file_info,2},
% {altname,1},
% {altname,2},
% {write_file_info,2},
% {write_file_info,3},
% {make_link,2},
% {make_link,3},
% {make_symlink,2},
% {make_symlink,3},
% {read_link,1},
% {read_link,2},
% {read_link_info,1},
% {read_link_info,2},
% {list_dir,1},
% {list_dir,2},
% {open_mode,1},
% {open_mode,4},
% {module_info,0},
% {module_info,1},
% {internal_normalize_utf8,1},
% {internal_native2name,1},
% {internal_name2native,1}]


-include_lib("kernel/include/file.hrl").
functions() ->
    [
%	advise,
%	pwrite,
%	datasync
%	sync
%	readline,
%	read_file,
%	delete,
	list_dir,
	read_file_info
    ].



benchmark_arguments() ->
    [{Functions, [Mod, N, [{type, Type}]]} ||
	Functions <- functions(),
	N         <- [100, 1000, 10000],
	Type      <- [list, binary],
	Mod	  <- [prim_file, file]
    ].

benchmark_unit() -> "ms".


init() ->
    file:set_cwd("/tmp"),
    ok.

datetime_to_string({{Year, Month, Day},{Hour, Min, Sec}}) ->
    lists:flatten(io_lib:format("~4.4.0w_~2.2.0w_~2.2.0w-~2.2.0w_~2.2.0w_~2.2.0w", [Year, Month, Day, Hour, Min, Sec])).


filename() ->
    "benchmark_test" ++ datetime_to_string({erlang:date(), erlang:time()}).


%%%%%%%%%%%%%%%%%% operations %%%%%%%%%%%%%%%%%%%%%%


%% list_dir

list_dir(Mod, N, _Opts) ->
    init(),
    % create a directory and some files in that directory
    Fname  = filename(),
    file:make_dir(Fname),
    Nfiles = 100,
    lists:foreach(fun
	    (I) ->
		Fname1 = Fname ++ integer_to_list(I),
		file:write_file(filename:join([Fname, Fname1]), <<"1">>)
	end, lists:seq(1, Nfiles)),
    T0   = now(),
    ok   = list_dir_loop(Mod, Fname, N),
    T1   = now(),
    %% clean up directory
    timer:now_diff(T1, T0) div 1000.

list_dir_loop(_, _, 0) -> ok;
list_dir_loop(Mod, Path, N) ->
    _ = Mod:list_dir(Path),
    list_dir_loop(Mod, Path, N - 1).


%% read_file_info

read_file_info(Mod, N, _Opts) ->
    init(),
    Path = filename(),
    file:make_dir(Path),
    File = filename:join([Path, "file_info_file"]),
    file:write_file(File, <<"1">>),
    T0 = now(),
    ok = read_file_info_loop(Mod, File, N),
    T1 = now(),
    timer:now_diff(T1, T0) div 1000.



%% aux

read_file_info_loop(_, _, 0) -> ok;
read_file_info_loop(Mod, File, N) ->
    {ok, _FI} = Mod:read_file_info(File),
    read_file_info_loop(Mod, File, N - 1).



%port_opts([]) -> [];
%port_opts([{type, binary} | Opts]) -> [binary | port_opts(Opts)];
%port_opts([_ | Opts]) -> port_opts(Opts).


