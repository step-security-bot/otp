%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2022. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(interactive_shell_SUITE).
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

%% Things to add tests for:
%%  - TERM=dumb
%%  - Editing line > MAXSIZE (1 << 16)
%%  - \t tests (use io:format("\t"))
%%  - xn fix after Delete and Backspace
%%  - octal_to_hex > 255 length (is this possible?)
%% 1222           0 :         } else if (lastput == 0) { /* A multibyte UTF8 character */
%% 1223           0 :             for (i = 0; i < ubytes; ++i) {
%% 1224           0 :                 outc(ubuf[i]);
%% 1225             :             }
%% 1226             :         } else {
%% 1227           0 :             outc(lastput);
%%  - $TERM set to > 1024 long value

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
	 get_columns_and_rows/1, exit_initial/1, job_control_local/1,
	 job_control_remote/1,stop_during_init/1,wrap/1,
         shell_history/1, shell_history_resize/1, shell_history_eaccess/1,
         shell_history_repair/1, shell_history_repair_corrupt/1,
         shell_history_corrupt/1,
         shell_history_custom/1, shell_history_custom_errors/1,
	 job_control_remote_noshell/1,ctrl_keys/1,
         get_columns_and_rows_escript/1,
         shell_navigation/1, shell_xnfix/1, shell_delete/1,
         shell_transpose/1, shell_search/1, shell_insert/1,
         shell_update_window/1, shell_huge_input/1,
         shell_invalid_unicode/1, shell_support_ansi_input/1,
         shell_invalid_ansi/1, shell_suspend/1, shell_full_queue/1,
         shell_unicode_wrap/1, shell_delete_unicode_wrap/1,
         shell_delete_unicode_not_at_cursor_wrap/1,
         shell_update_window_unicode_wrap/1,
         remsh_basic/1, remsh_longnames/1, remsh_no_epmd/1]).

%% For spawn
-export([toerl_server/4]).
%% Exports for custom shell history module
-export([load/0, add/1]).
%% For custom prompt testing
-export([prompt/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() ->
    [{group, to_erl},
     {group, tty}].

groups() ->
    [{to_erl,[],
      [get_columns_and_rows_escript,get_columns_and_rows,
       exit_initial, job_control_local,
       job_control_remote, job_control_remote_noshell,
       ctrl_keys, stop_during_init, wrap,
       {group, shell_history},
       {group, remsh}]},
     {shell_history, [],
      [shell_history,
       shell_history_resize,
       shell_history_eaccess,
       shell_history_repair,
       shell_history_repair_corrupt,
       shell_history_corrupt,
       {group, sh_custom}
      ]},
     {sh_custom, [],
      [shell_history_custom,
       shell_history_custom_errors]},
     {remsh, [],
      [remsh_basic,
       remsh_longnames,
       remsh_no_epmd]},
     {tty,[],
      [{group,tty_unicode},
       {group,tty_latin1},
       shell_suspend,
       shell_full_queue
      ]},
     {tty_unicode,[parallel],
      [{group,tty_tests},
       shell_invalid_unicode,
       shell_unicode_wrap,
       shell_delete_unicode_wrap,
       shell_delete_unicode_not_at_cursor_wrap,
       shell_update_window_unicode_wrap
      ]},
     {tty_latin1,[],[{group,tty_tests}]},
     {tty_tests, [parallel],
      [shell_navigation, shell_xnfix, shell_delete,
       shell_transpose, shell_search, shell_insert,
       shell_update_window, shell_huge_input,
       shell_support_ansi_input, shell_invalid_ansi]}
    ].

init_per_suite(Config) ->
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM", "vt100"),
    [{term,Term}|Config].

end_per_suite(Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(to_erl, Config) ->
    case get_progs() of
        {error, Error} ->
            {skip, Error};
        _ ->
            DefShell = get_default_shell(),
            [{default_shell,DefShell}|Config]
    end;
init_per_group(remsh, Config) ->
    case proplists:get_value(default_shell, Config) of
        old -> {skip, "Not supported in old shell"};
        new -> Config
    end;
init_per_group(shell_history, Config) ->
    case proplists:get_value(default_shell, Config) of
        old -> {skip, "Not supported in old shell"};
        new -> Config
    end;
init_per_group(tty, Config) ->
    case string:split(tmux("-V")," ") of
        ["tmux",[Num,$.|_]] when Num >= $3, Num =< $9 ->
            tmux("kill-session"),
            "" = tmux("-u new-session -x 50 -y 60 -d"),
            ["" = tmux(["set-environment '",Name,"' '",Value,"'"])
             || {Name,Value} <- os:env()],
            Config;
        ["tmux", Vsn] ->
            {skip, "invalid tmux version " ++ Vsn ++ ". Need vsn 3 or later"};
        Error ->
            {skip, "tmux not installed " ++ Error}
    end;
init_per_group(Group, Config) when Group =:= tty_unicode;
                                   Group =:= tty_latin1 ->
    [Lang,_] =
        string:split(
          os:getenv("LC_ALL",
                    os:getenv("LC_CTYPE",
                              os:getenv("LANG","en_US.UTF-8"))),"."),
    case Group of
        tty_unicode ->
            [{encoding, unicode},{env,[{"LC_ALL",Lang++".UTF-8"}]}|Config];
        tty_latin1 ->
            [{encoding, latin1},{env,[{"LC_ALL",Lang++".ISO-8859-1"}]}|Config]
    end;
init_per_group(sh_custom, Config) ->
    %% Ensure that ERL_AFLAGS will not override the value of the shell_history variable.
    {ok, Peer, Node} = ?CT_PEER(["-noshell","-kernel","shell_history","not_overridden"]),
    try erpc:call(Node, application, get_env, [kernel, shell_history], timeout(normal)) of
        {ok, not_overridden} ->
            Config;
        _ ->
            SkipText = "shell_history variable is overridden (probably by ERL_AFLAGS)",
            {skip, SkipText}
    catch
        C:R:Stk ->
            io:format("~p\n~p\n~p\n", [C,R,Stk]),
            {skip, "Unexpected error"}
    after
        peer:stop(Peer)
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(tty, _Config) ->
    Windows = string:split(tmux("list-windows"), "\n", all),
    lists:foreach(
      fun(W) ->
              case string:split(W, " ", all) of
                  ["0:" | _] -> ok;
                  [No, _Name | _] ->
                      "" = os:cmd(["tmux select-window -t ", string:split(No,":")]),
                      ct:log("~ts~n~ts",[W, os:cmd(lists:concat(["tmux capture-pane -p -e"]))])
              end
      end, Windows),
%    "" = os:cmd("tmux kill-session")
    ok;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) ->
    Path = [Func,
            [proplists:get_value(name,P) ||
                P <- [proplists:get_value(tc_group_properties,Config,[])] ++
                    proplists:get_value(tc_group_path,Config,[])]],
    [{tc_path, lists:concat(lists:join("-",lists:flatten(Path)))} | Config].

end_per_testcase(_Case, Config) ->
    case proplists:get_value(name, proplists:get_value(tc_group_properties, Config)) of
        tty_tests -> ok;
        _ ->
            %% Terminate any connected nodes. They may disturb test cases that follow.
            lists:foreach(fun(Node) ->
                                  catch erpc:call(Node, erlang, halt, [])
                          end, nodes()),
            ok
    end.

%%-define(DEBUG,1).
-ifdef(DEBUG).
-define(dbg(Data),ct:pal("~p",[Data])).
-else.
-define(dbg(Data),noop).
-endif.

string_to_term(Str) ->
    {ok,Tokens,_EndLine} = erl_scan:string(Str ++ "."),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

run_unbuffer_escript(Rows, Columns, EScript, NoTermStdIn, NoTermStdOut) ->
    DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
    TmpFile = filename:join(DataDir, "tmp"),
    ok = file:write_file(TmpFile, <<>>),
    CommandModifier =
        case {NoTermStdIn, NoTermStdOut} of
            {false, false} -> "";
            {true, false} -> io_lib:format(" < ~s", [TmpFile]);
            {false, true} -> io_lib:format(" > ~s ; cat ~s", [TmpFile, TmpFile]);
            {true, true} -> io_lib:format(" > ~s < ~s ; cat ~s", [TmpFile, TmpFile, TmpFile])
        end,
    Command = io_lib:format("unbuffer -p bash -c \"stty rows ~p; stty columns ~p; escript ~s ~s\"",
                               [Rows, Columns, EScript, CommandModifier]),
    %% io:format("Command: ~s ~n", [Command]),
    Out = os:cmd(Command),
    %% io:format("Out: ~p ~n", [Out]),
    string_to_term(Out).

get_columns_and_rows_escript(Config) when is_list(Config) ->
    ExpectUnbufferInstalled =
        try
            "79" = string:trim(os:cmd("unbuffer -p bash -c \"stty columns 79 ; tput cols\"")),
            true
        catch
            _:_ -> false
        end,
    case ExpectUnbufferInstalled of
        false ->
            {skip,
             "The unbuffer tool (https://core.tcl-lang.org/expect/index) does not seem to be installed.~n"
             "On Ubuntu/Debian: \"sudo apt-get install expect\""};
        true ->
            DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
            IoColumnsErl = filename:join(DataDir, "io_columns.erl"),
            IoRowsErl = filename:join(DataDir, "io_rows.erl"),
            [
             begin
                 {ok, 42} = run_unbuffer_escript(99, 42, IoColumnsErl, NoTermStdIn, NoTermStdOut),
                 {ok, 99} = run_unbuffer_escript(99, 42, IoRowsErl, NoTermStdIn, NoTermStdOut)
             end
             ||
                {NoTermStdIn, NoTermStdOut} <- [{false, false}, {true, false}, {false, true}]
            ],
            {error,enotsup} = run_unbuffer_escript(99, 42, IoRowsErl, true, true),
            {error,enotsup} = run_unbuffer_escript(99, 42, IoColumnsErl, true, true),
            ok
    end.

%% Test that the shell can access columns and rows.
get_columns_and_rows(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
            test_columns_and_rows(old, []);
	new ->
            test_columns_and_rows(old, ["-oldshell"]),
            test_columns_and_rows(new, [])
    end,
    ok.

test_columns_and_rows(old, Args) ->
    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "io:columns()."},
            {expect, "{error,enotsup}\r\n"},
            {putline, "io:rows()."},
            {expect, "{error,enotsup}\r\n"}
           ], [], [], Args),

    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "io:columns()."},
            {expect, "{ok,90}\r\n"},
            {putline,"io:rows()."},
            {expect, "{ok,40}\r\n"}],
           [],
           "stty rows 40; stty columns 90; ",
           Args);
test_columns_and_rows(new, _Args) ->
    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect, "\r\n2\r\n"},
            {expect, "> $"},
            {putline, "io:columns()."},
            {expect, "{ok,80}\r\n"},
            {expect, "> $"},
            {putline, "io:rows()."},
            {expect, "\r\n{ok,24}\r\n"}
           ]),

    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect,  "\r\n2\r\n"},
            {expect, "> $"},
            {putline, "io:columns()."},
            {expect, "\r\n{ok,90}\r\n"},
            {expect, "> $"},
            {putline, "io:rows()."},
            {expect, "\r\n{ok,40}\r\n"}],
           [],
           "stty rows 40; stty columns 90; ").

shell_navigation(Config) ->

    Term = start_tty(Config),

    try
        [begin
             send_tty(Term,"{aaa,'b"++U++"b',ccc}"),
             check_location(Term, {0, 0}), %% Check that cursor jump backward
             check_content(Term, "{aaa,'b"++U++"b',ccc}$"),
             timer:sleep(1000), %% Wait for cursor to jump back
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"Home"),
             check_location(Term, {0, 0}),
             send_tty(Term,"End"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"Left"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,")}),
             send_tty(Term,"C-Right"),
             check_location(Term, {0, width("{aaa,'b"++U++"b'")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, 0}),
             send_tty(Term,"C-E"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"C-A"),
             check_location(Term, {0, 0}),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()],
        ok
    after
        stop_tty(Term)
    end.

shell_xnfix(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    As = lists:duplicate(Cols - Col - 1,"a"),

    try
        [begin
             check_location(Term, {0, 0}),
             send_tty(Term,As),
             check_content(Term,[As,$$]),
             check_location(Term, {0, Cols - Col - 1}),
             send_tty(Term,"a"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"aaa"),
             check_location(Term, {0, -Col + 3}),
             [send_tty(Term,"Left") || _ <- lists:seq(1,3 + width(U))],
             send_tty(Term,U),
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U),$a),
                                 U,"\n",lists:duplicate(3+width(U), $a),"$"]),
             check_location(Term, {0, -Col}),
             send_tty(Term,"Left"),
             send_tty(Term,U),
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U),$a),
                                 U,"\n",U,lists:duplicate(3+width(U), $a),"$"]),
             check_location(Term, {0, -Col}),
             send_tty(Term,"Left"),
             send_tty(Term,"BSpace"),
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U) - 1,$a),
                                 U,"\n",U,lists:duplicate(3+width(U), $a),"$"]),
             send_tty(Term,"BSpace"),
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U) - 1,$a),
                                 U,U,"\n",lists:duplicate(3+width(U), $a),"$"]),
             send_tty(Term,"aa"),
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U) - 1,$a),
                                 U,"a\n",U,lists:duplicate(3+width(U), $a),"$"]),
             check_location(Term, {0, -Col}),
             send_tty(Term,"C-K"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"C-A"),
             check_location(Term, {-1, 0}),
             send_tty(Term,"C-E"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"Enter"),
             ok
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.


%% Characters that are larger than 2 wide need special handling when they
%% are at the end of the current line.
shell_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             FirstLine = [U,lists:duplicate(Cols - Col - width(U)*2 + 1,"a")],
             OtherLineA = [U,lists:duplicate(Cols - width(U) * 2+1,"a")],
             OtherLineB = [U,lists:duplicate(Cols - width(U) * 2+1,"b")],
             OtherLineC = [U,lists:duplicate(Cols - width(U) * 2+1,"c")],
             OtherLineD = [U,lists:duplicate(Cols - width(U) * 2+1,"d")],
             send_tty(Term,FirstLine),
             check_content(Term, [FirstLine,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineA),
             check_content(Term, [OtherLineA,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineB),
             check_content(Term, [OtherLineB,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineC),
             check_content(Term, [OtherLineC,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineD),
             check_content(Term, [OtherLineD,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,"C-A"),
             check_location(Term, {-4, 0}), %% Broken
             send_tty(Term,"Right"),
             check_location(Term, {-4, width(U)}), %% Broken

             send_tty(Term,"DC"), %% Broken
             check_content(Term, ["a.*",U,"$"]),
             check_content(Term, ["^b.*",U,"c$"]),
             check_content(Term, ["^c.*",U,"dd$"]),

             send_tty(Term,"a"),
             check_content(Term, [FirstLine,$$]),
             check_content(Term, [OtherLineA,$$]),
             check_content(Term, [OtherLineB,$$]),
             check_content(Term, [OtherLineC,$$]),
             check_content(Term, [OtherLineD,$$]),

             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

shell_delete(Config) ->

    Term = start_tty(Config),

    try

        [ begin
              send_tty(Term,"a"),
              check_content(Term, "> a$"),
              check_location(Term, {0, 1}),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, 0}),
              check_content(Term, ">$"),
              send_tty(Term,"a"),
              send_tty(Term,U),
              check_location(Term, {0, width([$a, U])}),
              send_tty(Term,"a"),
              send_tty(Term,U),
              check_location(Term, {0, width([$a,U,$a,U])}),
              check_content(Term, ["> a",U,$a,U,"$"]),
              send_tty(Term,"Left"),
              send_tty(Term,"Left"),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, width([$a])}),
              check_content(Term, ["> aa",U,"$"]),
              send_tty(Term,U),
              check_location(Term, {0, width([$a,U])}),
              send_tty(Term,"Left"),
              send_tty(Term,"DC"),
              check_location(Term, {0, width([$a])}),
              check_content(Term, ["> aa",U,"$"]),
              send_tty(Term,"DC"),
              send_tty(Term,"DC"),
              check_content(Term, ["> a$"]),
              send_tty(Term,"C-E"),
              check_location(Term, {0, width([$a])}),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, width([])})
          end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters at the edge of the screen that are "large",
%% we need to take special care.
shell_delete_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,[U,U,"aaaaa"]),
             check_content(Term,["\n",U,U,"aaaaa$"]),
             [send_tty(Term,"Left") || _ <- lists:seq(1,5+2)],
             check_location(Term,{0,-Col}),
             send_tty(Term,"BSpace"),
             check_content(Term,"> a* \n"),
             check_location(Term,{-1,Cols - Col - 1}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,"\n"]),
             check_location(Term,{-1,Cols - Col - 2}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U," \n"]),
             check_location(Term,{-1,Cols - Col - 3}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,U,"\n"]),
             check_content(Term,["\naaaaa$"]),
             check_location(Term,{-1,Cols - Col - 4}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,U,"a\n"]),
             check_content(Term,["\naaaa$"]),
             check_location(Term,{-1,Cols - Col - 5}),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters and a "large" characters is changing line we need
%% to take extra care
shell_delete_unicode_not_at_cursor_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,["a",U,"aaaaa"]),
             check_content(Term,["\na",U,"aaaaa$"]),
             send_tty(Term,"C-A"),
             send_tty(Term,"DC"),
             check_content(Term,["\n",U,"aaaaa$"]),
             send_tty(Term,"DC"),
             check_content(Term,["\n",U,"aaaaa$"]),
             check_content(Term,["> a* \n"]),
             send_tty(Term,"DC"),
             check_content(Term,["\naaaaa$"]),
             check_content(Term,["> a*",U,"\n"]),
             send_tty(Term,"DC"),
             check_content(Term,["\naaaa$"]),
             check_content(Term,["> a*",U,"a\n"]),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters and a "large" characters is changing line we need
%% to take extra care
shell_update_window_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col - width(U) + 1,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,[U,"aaaaa"]),
             check_content(Term,["> a* ?\n",U,"aaaaa$"]),
             tmux(["resize-window -t ",tty_name(Term)," -x ",Cols+1]),
             check_content(Term,["> a*",U,"\naaaaa$"]),
             tmux(["resize-window -t ",tty_name(Term)," -x ",Cols]),
             check_content(Term,["> a* ?\n",U,"aaaaa$"]),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

shell_transpose(Config) ->

    Term = start_tty(Config),

    Unicode = [[$a]] ++ hard_unicode(),

    try
        [
         begin
             send_tty(Term,"a"),
             [send_tty(Term,[CP]) || CP <- U],
             send_tty(Term,"b"),
             [[send_tty(Term,[CP]) || CP <- U2] || U2 <- Unicode],
             send_tty(Term,"cde"),
             check_content(Term, ["a",U,"b",Unicode,"cde$"]),
             check_location(Term, {0, width(["a",U,"b",Unicode,"cde"])}),
             send_tty(Term,"Home"),
             check_location(Term, {0, 0}),
             send_tty(Term,"Right"),
             send_tty(Term,"Right"),
             check_location(Term, {0, 1+width([U])}),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",U,Unicode,"cde$"]),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",hd(Unicode),U,tl(Unicode),"cde$"]),
             [send_tty(Term,"C-T") || _ <- lists:seq(1,length(Unicode)-1)],
             check_content(Term, ["ab",Unicode,U,"cde$"]),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",Unicode,"c",U,"de$"]),
             check_location(Term, {0, width(["ab",Unicode,"c",U])}),
             send_tty(Term,"End"),
             check_location(Term, {0, width(["ab",Unicode,"c",U,"de"])}),
             send_tty(Term,"Left"),
             send_tty(Term,"Left"),
             send_tty(Term,"BSpace"),
             check_content(Term, ["ab",Unicode,"cde$"]),
             send_tty(Term,"End"),
             send_tty(Term,"Enter")
         end || U <- Unicode],
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_search(C) ->

    Term = start_tty(C),
    {_Row, Cols} = get_location(Term),

    try
        send_tty(Term,"a"),
        send_tty(Term,"."),
        send_tty(Term,"Enter"),
        send_tty(Term,"'"),
        send_tty(Term,"a"),
        send_tty(Term,[16#1f600]),
        send_tty(Term,"'"),
        send_tty(Term,"."),
        send_tty(Term,"Enter"),
        check_location(Term, {0, 0}),
        send_tty(Term,"C-r"),
        check_location(Term, {0, - Cols + width(C, "(search)`': 'a😀'.") }),
        send_tty(Term,"C-a"),
        check_location(Term, {0, width(C, "'a😀'.")}),
        send_tty(Term,"Enter"),
        send_tty(Term,"C-r"),
        check_location(Term, {0, - Cols + width(C, "(search)`': 'a😀'.") }),
        send_tty(Term,"a"),
        check_location(Term, {0, - Cols + width(C, "(search)`a': 'a😀'.") }),
        send_tty(Term,"C-r"),
        check_location(Term, {0, - Cols + width(C, "(search)`a': a.") }),
        send_tty(Term,"BSpace"),
        check_location(Term, {0, - Cols + width(C, "(search)`': 'a😀'.") }),
        send_tty(Term,"BSpace"),
        check_location(Term, {0, - Cols + width(C, "(search)`': 'a😀'.") }),
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_insert(Config) ->
    Term = start_tty(Config),

    try
        send_tty(Term,"abcdefghijklm"),
        check_content(Term, "abcdefghijklm$"),
        check_location(Term, {0, 13}),
        send_tty(Term,"Home"),
        send_tty(Term,"Right"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        check_content(Term, "bcdeafghijklm$"),
        send_tty(Term,"End"),
        send_tty(Term,"Left"),
        send_tty(Term,"Left"),
        send_tty(Term,"BSpace"),
        check_content(Term, "bcdeafghijlm$"),
        ok
    after
        stop_tty(Term)
    end.

shell_update_window(Config) ->
    Term = start_tty(Config),

    Text = lists:flatten(["abcdefghijklmabcdefghijklm"]),
    {_Row, Col} = get_location(Term),

    try
        send_tty(Term,Text),
        check_content(Term,Text),
        check_location(Term, {0, width(Text)}),
        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text)+Col+1]),
        send_tty(Term,"a"),
        check_location(Term, {0, -Col}),
        send_tty(Term,"BSpace"),
        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text)+Col]),
        %% xnfix bug! at least in tmux... seems to work in iTerm as it does not
        %% need xnfix when resizing
        check_location(Term, {0, -Col}),
        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text) div 2 + Col]),
        check_location(Term, {0, -Col + width(Text) div 2}),
        ok
    after
        stop_tty(Term)
    end.

shell_huge_input(Config) ->
    Term = start_tty(Config),

    ManyUnicode = lists:duplicate(100,hard_unicode()),

    try
        send_tty(Term,ManyUnicode),
        check_content(Term, hard_unicode_match(Config) ++ "$",
                      #{ replace => {"\n",""} }),
        send_tty(Term,"Enter"),
        ok
    after
        stop_tty(Term)
    end.

%% Test that the shell works when invalid utf-8 (aka latin1) is sent to it
shell_invalid_unicode(Config) ->
    Term = start_tty(Config),

    InvalidUnicode = <<$å,$ä,$ö>>, %% åäö in latin1

    try
        send_tty(Term,hard_unicode()),
        check_content(Term, hard_unicode() ++ "$"),
        send_tty(Term,"Enter"),
        %% Send invalid utf-8
        send_stdin(Term,InvalidUnicode),
        %% Check that the utf-8 was echoed
        check_content(Term, "\\\\345\\\\344\\\\366$"),
        send_tty(Term,"Enter"),
        %% Check that the terminal entered "latin1" mode
        send_tty(Term,"😀한."),
        check_content(Term, "\\Q\\360\\237\\230\\200\\355\\225\\234.\\E$"),
        send_tty(Term,"Enter"),
        %% Check that we can reset the encoding to unicode
        send_tty(Term,"io:setopts([{encoding,unicode}])."),
        send_tty(Term,"Enter"),
        send_tty(Term,"😀한"),
        check_content(Term, "😀한$"),
        ok
    after
        stop_tty(Term),
        ok
    end.


%% Test the we can handle ansi insert, navigation and delete
%%   We currently can not so skip this test
shell_support_ansi_input(Config) ->
    Term = start_tty(Config),

    BoldText = "\e[;1m",
    ClearText = "\e[0m",

    try
        send_stdin(Term,["{",BoldText,"a😀b",ClearText,"}"]),
        check_location(Term, {0, width("{a😀b}")}),
        check_content(Term, ["{", BoldText, "a😀b", ClearText, "}"]),
        send_tty(Term,"Left"),
        send_tty(Term,"Left"),
        check_location(Term, {0, width("{a😀")}),
        send_tty(Term,"C-Left"),
        check_location(Term, {0, width("{")}),
        send_tty(Term,"End"),
        send_tty(Term,"BSpace"),
        send_tty(Term,"BSpace"),
        check_content(Term, ["{", BoldText, "a😀"]),
        ok
    after
        stop_tty(Term),
        ok
    end.

%% Test the we can handle invalid ansi escape chars.
%%   tmux cannot handle this... so maybe we need to test it using to_erl?
shell_invalid_ansi(Config) ->

    InvalidAnsi = fun() -> ["\e]94m"] end,

    Term = start_tty(Config),

    try
        rpc(Term, application, set_env,
            [stdlib, shell_prompt_func_test, InvalidAnsi]),
        send_tty(Term, "Enter"),
        check_content(Term,"\\Q(\e]94m)2>\\E$"),
        ok
    after
        stop_tty(Term),
        ok
    end.


%% We test that suspending of `erl` and then resuming restores the shell
shell_suspend(Config) ->

    Name = peer:random_name(proplists:get_value(tc_path,Config)),
    %% In order to suspend `erl` we need it to run in a shell that has job control
    %% so we start the peer within a tmux window instead of having it be the original
    %% process.
    os:cmd("tmux new-window -n " ++ Name ++ " -d -- bash --norc"),

    Peer = #{ name => Name,
              post_process_args =>
                        fun(["new-window","-n",_,"-d","--"|CmdAndArgs]) ->
                                FlatCmdAndArgs =
                                      lists:join(
                                        " ",[[$',A,$'] || A <- CmdAndArgs]),
                                ["send","-t",Name,lists:flatten(FlatCmdAndArgs),"Enter"]
                        end
            },


    Term = start_tty([{peer, Peer}|Config]),

    try
        send_tty(Term, hard_unicode()),
        check_content(Term,["2> ",hard_unicode(),"$"]),
        send_tty(Term, "C-Z"),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        send_tty(Term, "C-L"),
        check_content(Term,["2> ",hard_unicode(),"$"]),
        check_location(Term,{0,width(hard_unicode())}),
        ok
    after
        stop_tty(Term),
        ok
    end.

%% We test that suspending of `erl` and then resuming restores the shell
shell_full_queue(Config) ->

    %% In order to fill the read buffer of the terminal we need to get a
    %% bit creative. We first need to start erl in bash in order to be
    %% able to get access to job control for suspended processes.
    %% We then also wrap `erl` in `unbuffer -p` so that we can suspend
    %% that program in order to block writing to stdout for a while.

    Name = peer:random_name(proplists:get_value(tc_path,Config)),
    os:cmd("tmux new-window -n " ++ Name ++ " -d -- bash --norc"),

    Peer = #{ name => Name,
              post_process_args =>
                        fun(["new-window","-n",_,"-d","--"|CmdAndArgs]) ->
                                FlatCmdAndArgs = ["unbuffer -p "] ++
                                      lists:join(
                                        " ",[[$',A,$'] || A <- CmdAndArgs]),
                                ["send","-t",Name,lists:flatten(FlatCmdAndArgs),"Enter"]
                        end
            },


    Term = start_tty([{peer, Peer}|Config]),

    UnbufferedPid = os:cmd("ps -o ppid= -p " ++ rpc(Term,os,getpid,[])),

    WriteUntilStopped =
        fun F() ->
                rpc(Term,io,format,[user,"a",[]]),
                put(bytes,get(bytes,0)+1),
                receive
                    stop ->
                        rpc(Term,io,format,[user,"b",[]])
                after 0 -> F()
                end
        end,

    WaitUntilBlocked =
        fun(Pid, Ref) ->
                (fun F(Cnt) ->
                         receive
                             {'DOWN',Ref,_,_,_} = Down ->
                                 ct:fail({io_format_did_not_block, Down})
                         after 1000 ->
                                 ok
                         end,
                         case process_info(Pid,dictionary) of
                             {dictionary,[{bytes,Cnt}]} ->
                                 ct:log("Bytes until blocked: ~p~n",[Cnt]),
                                 %% Add one extra byte as for
                                 %% the current blocking call
                                 Cnt + 1;
                             {dictionary,[{bytes,NewCnt}]} ->
                                 F(NewCnt)
                         end
                 end)(0)
        end,

    try
        %% First test that we can suspend and then resume
        os:cmd("kill -TSTP " ++ UnbufferedPid),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        {Pid, Ref} = spawn_monitor(WriteUntilStopped),
        WaitUntilBlocked(Pid, Ref),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        Pid ! stop,
        check_content(Term,"b$"),

        %% Then we test that all characters are written when system
        %% is terminated just after writing
        {ok,Cols} = rpc(Term,io,columns,[user]),
        send_tty(Term, "Enter"),
        os:cmd("kill -TSTP " ++ UnbufferedPid),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        {Pid2, Ref2} = spawn_monitor(WriteUntilStopped),
        Bytes = WaitUntilBlocked(Pid2, Ref2),
        stop_tty(Term),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        check_content(
          fun() ->
                  tmux(["capture-pane -p -S - -E - -t ",tty_name(Term)])
          end, lists:flatten([lists:duplicate(Cols,$a) ++ "\n" ||
                                 _ <- lists:seq(1,(Bytes) div Cols)]
                             ++ [lists:duplicate((Bytes) rem Cols,$a)])),
        ct:log("~ts",[tmux(["capture-pane -p -S - -E - -t ",tty_name(Term)])]),
        ok
    after
        stop_tty(Term),
        ok
    end.

get(Key,Default) ->
    case get(Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% A list of unicode graphemes that are notoriously hard to render
hard_unicode() ->
    ZWJ =
        case os:type() of
            %% macOS has very good rendering of ZWJ,
            %% but the cursor does not agree with it..
            {unix, darwin} -> [];
            _ -> [[16#1F91A,16#1F3FC]]    % Hand with skintone 🤚🏼
        end,
    [[16#1f600],             % Smilie 😀
    "한",                   % Hangul
    "Z̤͔ͧ̑̓","ä͖̭̈̇","lͮ̒ͫ","ǧ̗͚̚","o̙̔ͮ̇͐̇" %% Vertically stacked chars
     %%"👩‍👩",            % Zero width joiner
     %%"👩‍👩‍👧‍👦"                % Zero width joiner
     | ZWJ].

hard_unicode_match(Config) ->
    ["\\Q",[unicode_to_octet(Config, U) || U <- hard_unicode()],"\\E"].

unicode_to_octet(Config, U) ->
    case ?config(encoding,Config) of
        unicode -> U;
        latin1 -> unicode_to_octet(U)
    end.

unicode_to_octet(U) ->
    [if Byte >= 128 -> [$\\,integer_to_list(Byte,8)];
        true -> Byte
     end || <<Byte>> <= unicode:characters_to_binary(U)].

unicode_to_hex(Config, U) ->
    case ?config(encoding,Config) of
        unicode -> U;
        latin1 -> unicode_to_hex(U)
    end.

unicode_to_hex(U) when is_integer(U) ->
    unicode_to_hex([U]);
unicode_to_hex(Us) ->
    [if U < 128 -> U;
        U < 512 -> ["\\",integer_to_list(U,8)];
        true -> ["\\x{",integer_to_list(U,16),"}"]
     end || U <- Us].

width(C, Str) ->
    case ?config(encoding, C) of
        unicode -> width(Str);
        latin1 -> width(unicode_to_octet(Str))
    end.
width(Str) ->
    lists:sum(
      [npwcwidth(CP) || CP <- lists:flatten(Str)]).

%% Poor mans character width
npwcwidth(16#D55C) ->
    2; %% 한
npwcwidth(16#1f91A) ->
    2; %% hand
npwcwidth(16#1F3Fc) ->
    2; %% Skintone
npwcwidth(16#1f600) ->
    2; %% smilie
npwcwidth(C) ->
    case lists:member(C, [775,776,780,785,786,787,788,791,793,794,
                          804,813,848,852,854,858,871,875,878]) of
        true ->
            0;
        false ->
            1
    end.

-record(tmux, {peer, node, name, orig_location }).

tmux([Cmd|_] = Command) when is_list(Cmd) ->
    tmux(lists:concat(Command));
tmux(Command) ->
    string:trim(os:cmd(["tmux ",Command])).

rpc(#tmux{ node = N }, M, F, A) ->
    erpc:call(N, M, F, A).

start_tty(Config) ->

    %% Start an node in an xterm
    %% {ok, XPeer, _XNode} = ?CT_PEER(#{ exec =>
    %%                                    {os:find_executable("xterm"),
    %%                                     ["-hold","-e",os:find_executable("erl")]},
    %%                                   detached => false }),

    Name = maps:get(name,proplists:get_value(peer, Config, #{}),
                    peer:random_name(proplists:get_value(tc_path, Config))),

    Envs = lists:flatmap(fun({Key,Value}) ->
                                 ["-env",Key,Value]
                         end, proplists:get_value(env,Config,[])),

    ExecArgs = case os:getenv("TMUX_DEBUG") of
                   "strace" ->
                       STraceLog = filename:join(proplists:get_value(priv_dir,Config),
                                                 Name++".strace"),
                       ct:pal("Link to strace: file://~ts", [STraceLog]),
                       [os:find_executable("strace"),"-f",
                        "-o",STraceLog,
                        "-e","trace=all",
                        "-e","read=0,1,2",
                        "-e","write=0,1,2"
                       ] ++ string:split(ct:get_progname()," ",all);
                   "rr" ->
                       [os:find_executable("cerl"),"-rr"];
                   _ ->
                       string:split(ct:get_progname()," ",all)
               end,
    DefaultPeerArgs = #{ name => Name,
                         exec =>
                             {os:find_executable("tmux"),
                              ["new-window","-n",Name,"-d","--"] ++ ExecArgs },

                         args => ["-pz",filename:dirname(code:which(?MODULE)),
                                  "-connect_all","false",
                                  "-kernel","logger_level","all",
                                  "-kernel","shell_history","disabled",
                                  "-kernel","prevent_overlapping_partitions","false",
                                  "-eval","shell:prompt_func({interactive_shell_SUITE,prompt})."
                                 ] ++ Envs,
                         detached => false
                       },

    {ok, Peer, Node} =
        ?CT_PEER(maps:merge(proplists:get_value(peer,Config,#{}),
                            DefaultPeerArgs)),

    Self = self(),

    %% By default peer links with the starter. For these TCs we however only
    %% want the peer to die if we die, so we create a "unidirection link" using
    %% monitors.
    spawn(fun() ->
                  TCRef = erlang:monitor(process, Self),
                  PeerRef = erlang:monitor(process, Peer),
                  receive
                      {'DOWN',TCRef,_,_,Reason} ->
                          exit(Peer, Reason);
                      {'DOWN',PeerRef,_,_,_} ->
                          ok
                  end
          end),
    unlink(Peer),

    Prompt = fun() -> ["\e[94m",54620,44397,50612,47,51312,49440,47568,"\e[0m"] end,
    erpc:call(Node, application, set_env,
              [stdlib, shell_prompt_func_test,
               proplists:get_value(shell_prompt_func_test, Config, Prompt)]),

    "" = tmux(["set-option -t ",Name," remain-on-exit on"]),
    Term = #tmux{ peer = Peer, node = Node, name = Name },
    {Rows, _} = get_window_size(Term),

    %% We send a lot of newlines here in order for the number of rows
    %% in the window to be max so that we can predict what the cursor
    %% position is.
    [send_tty(Term,"\n") || _ <- lists:seq(1, Rows)],

    %% We start tracing on the remote node in order to help debugging
    TraceLog = filename:join(proplists:get_value(priv_dir,Config),Name++".trace"),
    ct:pal("Link to trace: file://~ts",[TraceLog]),

    spawn(Node,
          fun() ->
                  {ok, TraceFile} = file:open(TraceLog, [write]),
                  dbg:tracer(
                    process,
                    {fun(Event, FD) ->
                             io:format(FD,"~tp~n", [Event]),
                             FD
                     end, TraceFile}
                   ),
                  dbg:p(whereis(user_drv),[c,m]),
                  dbg:tp(user_drv,x),
                  monitor(process, Self),
                  receive _ -> ok end
          end),

    %% We enter an 'a' here so that we can get the correct orig position
    %% with an alternative prompt.
    send_tty(Term,"a.\n"),
    check_content(Term,"2>$"),
    OrigLocation = get_location(Term),
    Term#tmux{ orig_location = OrigLocation }.

prompt(L) ->
    N = proplists:get_value(history, L, 0),
    Fun = application:get_env(stdlib, shell_prompt_func_test,
                              fun() -> atom_to_list(node()) end),
    io_lib:format("(~ts)~w> ",[Fun(),N]).

stop_tty(Term) ->
    catch peer:stop(Term#tmux.peer),
    ct:log("~ts",[get_content(Term)]),
%    "" = tmux("kill-window -t " ++ Term#tmux.name),
    ok.

tty_name(Term) ->
    Term#tmux.name.

send_tty(Term, "Home") ->
    %% https://stackoverflow.com/a/55616731
    send_tty(Term,"Escape"),
    send_tty(Term,"OH");
send_tty(Term, "End") ->
    send_tty(Term,"Escape"),
    send_tty(Term,"OF");
send_tty(#tmux{ name = Name } = _Term,Value) ->
    [Head | Quotes] = string:split(Value, "'", all),
    "" = tmux("send -t " ++ Name ++ " '" ++ Head ++ "'"),
    [begin
         "" = tmux("send -t " ++ Name ++ " \"'\""),
         "" = tmux("send -t " ++ Name ++ " '" ++ V ++ "'")
     end || V <- Quotes].

%% We use send_stdin for testing of things that we cannot sent via
%% the tmux send command, such as invalid unicode
send_stdin(Term, Chars) when is_binary(Chars) ->
    rpc(Term,erlang,display_string,[stdin,Chars]);
send_stdin(Term, Chars) ->
    send_stdin(Term, iolist_to_binary(unicode:characters_to_binary(Chars))).

check_location(Term, Where) ->
    check_location(Term, Where, 5).
check_location(#tmux{ orig_location = {OrigRow, OrigCol} = Orig } = Term,
               {AdjRow, AdjCol} = Where, Attempt) ->
    NewLocation = get_location(Term),
    case {OrigRow+AdjRow,OrigCol+AdjCol} of
        NewLocation -> NewLocation;
        _ when Attempt =:= 0 ->
            {NewRow, NewCol} = NewLocation,
            ct:fail({wrong_location, {expected,{AdjRow, AdjCol}},
                     {got,{NewRow - OrigRow, NewCol - OrigCol},
                      {NewLocation, Orig}}});
        _ ->
            timer:sleep(50),
            check_location(Term, Where, Attempt -1)
    end.

get_location(Term) ->
    RowAndCol = tmux("display -pF '#{cursor_y} #{cursor_x}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

get_window_size(Term) ->
    RowAndCol = tmux("display -pF '#{window_height} #{window_width}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

check_content(Term, Match) ->
    check_content(Term, Match, #{}).
check_content(Term, Match, Opts) when is_map(Opts) ->
    check_content(Term, Match, Opts, 5).
check_content(Term, Match, Opts, Attempt) ->
    OrigContent = case Term of
                  #tmux{} -> get_content(Term);
                  Fun when is_function(Fun,0) -> Fun()
              end,
    {RE,Repl} = maps:get(replace, Opts, {"",""}),
    Content = re:replace(OrigContent, RE, Repl, [global]),
    case re:run(string:trim(Content, both), lists:flatten(Match), [unicode]) of
        {match,_} ->
            ok;
        _ when Attempt =:= 0 ->
            io:format("Failed to find '~ts' in ~n'~ts'~n",
                      [unicode:characters_to_binary(Match), Content]),
            io:format("Failed to find '~w' in ~n'~w'~n",
                      [unicode:characters_to_binary(Match), Content]),
            ct:fail(nomatch);
        _ ->
            timer:sleep(500),
            check_content(Term, Match, Opts, Attempt - 1)
    end.

get_content(#tmux{ name = Name }) ->
    Content = unicode:characters_to_binary(tmux("capture-pane -p -t " ++ Name)),
    case string:split(Content,"a.\na") of
        [_Ignore,C] ->
            C;
        [C] ->
            C
    end.

%% Tests that exit of initial shell restarts shell.
exit_initial(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
            test_exit_initial(old);
        new ->
            test_exit_initial(old),
            test_exit_initial(new)
    end,
    ok.

test_exit_initial(old) ->
    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "exit()."},
            {expect, "Eshell"},
            {putline, ""},
            {putline, "35."},
            {expect, "35\r\n"}],
           [], [], ["-oldshell"]);
test_exit_initial(new) ->
    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect, "2"},
            {putline,"exit()."},
            {expect, "Eshell"},
            {expect, "1> $"},
            {putline, "35."},
            {expect, "35\r\n"}]).

stop_during_init(Config) when is_list(Config) ->
    {RunErl,_ToErl,[Erl|ErlArgs]} = get_progs(),
    case create_tempdir() of
        {error, Reason} ->
            {skip, Reason};
        Tempdir ->
            XArg = " -kernel shell_history enabled -s init stop",
            start_runerl_command(RunErl, Tempdir, "\\\""++Erl++"\\\""++ErlArgs++XArg),
            Logs = rtnode_read_logs(Tempdir),
            rtnode_dump_logs(Logs),
            nomatch = binary:match(map_get("erlang.log.1", Logs),
                                   <<"*** ERROR: Shell process terminated! ***">>),
            ok
    end.

%% This testcase tests that the correct wrapping characters are added
%% When a terminal has the xn flag set, it means that wrapping may not
%% work as expected and historically the ttysl driver has always inserted
%% a " \b" (i.e. space + backspace) when an output string ends on that line
%% in order for the cursor to be at col 0 on the next line instead of col max
%% on the current line.
%%
%% This caused problems when a string was `columns` long and then ended in "\r\n"
%% as it would first wrap due to " \b" and then output "\r\n" that cause a double
%% newline to happen.
%%
%% This testcase tests that we get a " \b" when we should and we get a "\r\n" when
%% we should.
wrap(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
        new ->
            As = lists:duplicate(20,"a"),
            rtnode([{putline, "io:columns()."},
                    {expect, "{ok,20}\r\n"},
                    {putline, ["io:format(\"~s\",[lists:duplicate(20,\"a\")])."]},
                    {expect, As ++ " \b"},
                    {putline, ["io:format(\"~s~n~s\",[lists:duplicate(20,\"a\"),lists:duplicate(20,\"a\")])."]},
                    {expect, As ++ "\r\n" ++ As ++ " \b"}
                   ],
                   [],
                   "stty rows 40; stty columns 20; ");
        _ ->
            ok
    end,
    ok.

%% This testcase tests that shell_history works as it should.
%% We use Ctrl + P = Cp=[$\^p] in order to navigate up
%% We use Ctrl + N = Cp=[$\^n] in order to navigate down
%% We use Ctrl + B = Cp=[$\^b] in order to navigate left
%% in the console. We also need to sleep for a while in order
%% for the system to update the display before we enter more
%% commands.
shell_history(Config) when is_list(Config) ->
    Path = shell_history_path(Config, "basic"),
    rtnode([
            {putline, "echo1."},
            {expect, "echo1\r\n"},
            {putline, "echo2."},
            {expect, "echo2\r\n"},
            {putline, "echo3."},
            {expect, "echo3\r\n"},
            {putline, "echo4."},
            {expect, "echo4\r\n"},
            {putline, "echo5."},
            {expect, "echo5\r\n"}
           ], [], [], mk_history_param(Path)),
    receive after 1000 -> ok end,
    rtnode([
            {sleep,100},
            {putline, ""},
            %% the init:stop that stopped the node is dropped
            {putdata, [$\^p]}, {expect, "echo5[.]$"},
            {putdata, [$\n]},
            {expect, "echo5\r\n"},
            {putdata, [$\^p]}, {expect, "echo5[.]$"},
            {putdata, [$\^p]}, {expect, "echo4[.]$"},
            {putdata, [$\^p]}, {expect, "echo3[.]$"},
            {putdata, [$\^p]}, {expect, "echo2[.]$"},
            {putdata, [$\^n]}, {expect, "echo3[.]$"},
            {putdata, [$\^n]}, {expect, "echo4[.]$"},
            {putdata, [$\^b]}, {sleep,50}, %% the echo4. (cursor moved one left)
            {putline, ["ECHO"]},
            {expect, "echo4ECHO\r\n"}
           ], [], [],
           mk_history_param(Path)),
    ok.

shell_history_resize(Config) ->
    Path = shell_history_path(Config, "resize"),
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"},
            {putline, "echo2."},
            {expect, "echo2\r\n"}
           ], [], [], ["-kernel","shell_history_file_bytes","123456"] ++
               mk_history_param(Path)),

    {ok, Logs} =
        rtnode([
                {sleep,100},
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo2[.]$$"},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo"}
               ], [], [], ["-kernel","shell_history_file_bytes","654321"] ++
                   mk_history_param(Path)),

    rtnode_check_logs(
      "erlang.log.1",
      "The configured log history file size is different from the size "
      "of the log file on disk", Logs),

    ok.

shell_history_eaccess(Config) ->
    Path = shell_history_path(Config, "eaccess"),
    file:make_dir(filename:dirname(Path)),
    ok = file:make_dir(Path),
    {ok, Info} = file:read_file_info(Path),
    try
        NoExecMode = Info#file_info.mode band (bnot 8#111),
        file:write_file_info(Path,Info#file_info{ mode = NoExecMode }),

        %% Cannot create history log in folder
        {ok, Logs1} =
            rtnode([
                    {putline, "echo."},
                    {expect, "echo\r\n"}
                   ], [], [], mk_history_param(Path)),

        ct:pal("~p",[Logs1]),
        rtnode_check_logs("erlang.log.1", "Error handling file", Logs1),

        %% shell_docs recursively creates the folder to store the
        %% logs. This test checks that erlang still starts if we
        %% cannot create the folders to the path.
        {ok, Logs2} =
            rtnode([
                    {putline, "echo."},
                    {expect, "echo\r\n"}
                   ], [], [], mk_history_param(filename:join(Path,"logs"))),

        rtnode_check_logs("erlang.log.1", "Error handling file", Logs2)

    after
        file:write_file_info(Path, Info)
    end,
    ok.

shell_history_repair(Config) ->
    Path = shell_history_path(Config, "repair"),

    %% We stop a node without closing the log
    shell_history_halt(Path),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], mk_history_param(Path)),

    %% The regexp below checks that he string is NOT part of the log
    rtnode_check_logs("erlang.log.1",
                      "The shell history log file was corrupted and was repaired",
                      false,
                      Logs),
    ok.

shell_history_repair_corrupt(Config) ->
    Path = shell_history_path(Config, "repair_corrupt"),

    %% We stop a node without closing the log
    shell_history_halt(Path),

    %% We corrupt the disklog
    {ok, D} = file:open(filename:join(Path,"erlang-shell-log.1"), [read,append]),
    ok = file:write(D, [10,10]),
    ok = file:close(D),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], mk_history_param(Path)),

    rtnode_check_logs("erlang.log.1",
                      "The shell history log file was corrupted and was repaired.",
                      Logs),
    ok.

shell_history_corrupt(Config) ->
    Path = shell_history_path(Config, "corrupt"),

    %% We initialize the shell history log with a known value.
    rtnode([{putline, "echo."},
            {expect, "echo\r\n"},
            {putline, "echo2."},
            {expect, "echo2\r\n"}
           ], [], [], mk_history_param(Path)),

    %% We corrupt the disklog.
    {ok, D} = file:open(filename:join(Path,"erlang-shell-log.1"), [read, append]),
    ok = file:write(D, [10, 10]),
    ok = file:close(D),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo2[.]$"},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], mk_history_param(Path)),

    rtnode_check_logs("erlang.log.1", "Invalid chunk in the file", Logs),
    ok.

%% Stop the node without closing the log.
shell_history_halt(Path) ->
    try
        rtnode([
                {putline, "echo."},
                {expect, "echo\r\n"},
                {sleep, 2500}, % disk_log internal cache timer is 2000 ms
                {putline, "halt(0)."},
                {expect, "\r\n"},
                {sleep, 1000} %% wait for node to terminate
               ], [], [], mk_history_param(Path))
    catch
        _:_ ->
            ok
    end.

shell_history_path(Config, TestCase) ->
        filename:join([proplists:get_value(priv_dir, Config),
                       "shell_history", TestCase]).

mk_history_param(Path) ->
    ["-kernel","shell_history","enabled",
     "-kernel","shell_history_path","\"" ++ Path ++ "\"",
     "-kernel","shell_history_drop","[\"init:stop().\"]"
    ].

shell_history_custom(_Config) ->
    %% Up key: Ctrl + P = Cp=[$\^p]
    rtnode([{expect, "1> $"},
            %% {putline, ""},
            {putdata, [$\^p]}, {expect, "0[.]"},
            {putdata, [$\n]},
            {expect, "0\r\n"},
            {putline, "echo."},
            {expect, "!echo\r\n"} % exclamation mark is printed by custom history module
           ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                       "-pz",filename:dirname(code:which(?MODULE))]),
    ok.

shell_history_custom_errors(_Config) ->

    %% Check that we can start with a node with an undefined
    %% provider module.
    rtnode([{putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], ["-kernel","shell_history","very_broken",
                       "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                       "-kernel","provider_load","crash",
                       "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                       "-kernel","provider_load","badreturn",
                       "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "Disabling shell history logging.\r\n"},
            {expect, "echo\r\n"}
           ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                       "-kernel","provider_add","crash",
                       "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "It returned {error,badreturn}.\r\n"},
            {expect, "echo\r\n"}
           ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                       "-kernel","provider_add","badreturn",
                       "-pz",filename:dirname(code:which(?MODULE))]),

    ok.

load() ->
    case application:get_env(kernel,provider_load) of
        {ok, crash} ->
            error(crash);
        {ok, badreturn} ->
            %% Should return a list of string()
            ok;
        _ ->
            ["0.\n\n"]
    end.

add(_Line) ->
    case application:get_env(kernel,provider_add) of
        {ok, crash} ->
            error(crash);
        {ok, badreturn} ->
            %% Should return ok
            {error, badreturn};
        _ ->
            io:format("!", [])
    end.

%% Tests that local shell can be started by means of job control.
job_control_local(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    %% Old shell tests
	    {skip,"No new shell found"};
	new ->
	    %% New shell tests
	    rtnode([{putline, ""},
                    {expect,  "1> $"},
		    {putline, "2."},
		    {expect,  "\r\n2\r\n"},
		    {putline, "\^g"},
                    {expect,  "--> $"},
		    {putline, "s"},
                    {expect,  "--> $"},
		    {putline, "c"},
                    {expect,  "\r\nEshell"},
                    {expect,  "1> $"},
		    {putline, "35."},
                    {expect,  "\r\n35\r\n"},
                    {expect,  "2> $"},
                    {putline, "receive M -> M end.\r\n"},
		    {putline, "\^g"},
                    {expect,  "--> $"},
                    {putline, "i 3"},
                    {expect,  "Unknown job"},
                    {expect,  "--> $"},
                    {putline, "i 2"},
                    {expect,  "--> $"},
                    {putline, "c"},
                    {expect,  "[*][*] exception exit: killed"},
                    {expect,  "[23]>"},
		    {putline, "\^g"},
                    {expect,  "--> $"},
                    {putline, "k 3"},
                    {expect,  "Unknown job"},
                    {expect,  "--> $"},
                    {putline, "k 2"},
                    {expect,  "--> $"},
                    {putline, "k"},
                    {expect,  "Unknown job"},
                    {expect,  "--> $"},
                    {putline, "c"},
                    {expect,  "Unknown job"},
                    {expect,  "--> $"},
                    {putline, "i"},
                    {expect,  "Unknown job"},
                    {expect,  "--> $"},
                    {putline, "?"},
                    {expect,  "this message"},
                    {expect,  "--> $"},
                    {putline, "h"},
                    {expect,  "this message"},
                    {expect,  "--> $"},
                    {putline, "c 1"},
                    {expect, "\r\n"},
                    {putline, "35."},
                    {expect, "\r\n35\r\n"},
                    {expect, "[23]> $"}
                   ]),
            ok
    end.

%% Tests that remote shell can be started by means of job control.
job_control_remote(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
            {ok, Peer, NSNode} = ?CT_PEER(#{ args => ["-connect_all","false"],
                                             peer_down => continue }),
            try
                test_remote_job_control(NSNode)
            after
                peer:stop(Peer)
            end
    end.

%% Tests that remote shell can be started by means of job control to
%% -noshell node.
job_control_remote_noshell(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
	    {ok, Peer, NSNode} = ?CT_PEER(#{ args => ["-connect_all","false","-noshell"],
                                             peer_down => continue }),
            try
                test_remote_job_control(NSNode)
            after
                peer:stop(Peer)
            end
    end.

test_remote_job_control(Node) ->
    RemNode = peer:random_name(),
    Pid = spawn_link(Node, fun() ->
                                   receive die ->
                                           ok
                                   end
                             end),
    PidStr = erpc:call(Node, erlang, pid_to_list, [Pid]),
    true = erpc:call(Node, erlang, register, [kalaskula,Pid]),
    PrintedNode = printed_atom(Node),
    CookieString = printed_atom(erlang:get_cookie()),

    rtnode([{putline, ""},
            {putline, "erlang:get_cookie()."},
            {expect, "\r\n\\Q" ++ CookieString ++ "\\E"},
            {putdata, "\^g"},
            {expect, " --> $"},
            {putline, "r " ++ PrintedNode},
            {expect, "\r\n"},
            {putline, "j"},
            {expect, "1  {shell,start,\\[init]}"},
            {expect, "2[*] {\\Q"++PrintedNode++"\\E,shell,start,\\[]}"},
            {expect, " --> $"},
            {putline, "c"},
            {expect, "\r\n"},
            {expect, "Eshell"},
            {expect, "\\Q(" ++ atom_to_list(Node) ++")1> \\E$"},
            {putline, "whereis(kalaskula)."},
            {expect, PidStr},
            {putline, "kalaskula ! die."},
            {putline, "exit()."},
            {expect, "[*][*][*] Shell process terminated!"},
            {putdata, "\^g"},
            {expect, " --> $"},
            {putline, "j"},
            {expect, "1  {shell,start,\\[init]}"},
            {expect, " --> $"},
            {putline, "c"},
            {expect, "Unknown job"},
            {expect, " --> $"},
            {putline, "c 1"},
            {expect, "\r\n"},
            {putline, ""},
            {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[12]> $"},
            {putdata, "\^g"},
            {expect, " --> $"},
            {putline, "j"},
            {expect, "1[*] {shell,start,\\[init]}"},
            {putline, "c"},
            {expect, "\r\n"},
            {sleep, 100},
            {putline, "35."},
            {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[123]> $"}
           ], RemNode),
    Pid ! die,
    ok.

%% Tests various control keys.
ctrl_keys(_Config) ->
    Cu = [$\^u],
    Cw = [$\^w],
    Cy = [$\^y],
    Home = [27,$O,$H],
    End = [27,$O,$F],
    rtnode([{putline,""},
	    {putline,"2."},
	    {expect,"2"},
	    {putline,"\"hello "++Cw++"world\"."},	% test <CTRL>+W
	    {expect,"\"world\""},
	    {putline,"\"hello "++Cu++"\"world\"."},	% test <CTRL>+U
	    {expect,"\"world\""},
	    {putline,"world\"."++Home++"\"hello "},	% test <HOME>
	    {expect,"\"hello world\""},
	    {putline,"world"++Home++"\"hello "++End++"\"."},	% test <END>
	    {expect,"\"hello world\""},
	    {putline,"\"hello world\""++Cu++Cy++"."},
	    {expect,"\"hello world\""}] ++
               wordLeft() ++ wordRight()),
    ok.

wordLeft() ->
    L1 = "\e\e[D",
    L2 = "\e[5D",
    L3 = "\e[1;5D",
    wordLeft(L1) ++ wordLeft(L2) ++ wordLeft(L3).

wordLeft(Chars) ->
    End = "\eOF",
    [{putline,"\"world\""++Chars++"hello "++End++"."},
     {expect,"\"hello world\""}].

wordRight() ->
    R1 = "\e\e[C",
    R2 = "\e[5C",
    R3 = "\e[1;5C",
    wordRight(R1) ++ wordRight(R2) ++ wordRight(R3).

wordRight(Chars) ->
    Home = "\eOH",
    [{putline,"world"++Home++"\"hello "++Chars++"\"."},
     {expect,"\"hello world\""}].

%% Test that -remsh works
remsh_basic(Config) when is_list(Config) ->
    {ok, Peer, TargetNode} = ?CT_PEER(),
    TargetNodeStr = printed_atom(TargetNode),
    [_Name,Host] = string:split(atom_to_list(node()), "@"),

    PreCmds = [{putline,""},
               {putline,"node()."},
               {expect, "\\Q" ++ TargetNodeStr ++ "\\E\r\n"}],

    PostCmds = quit_hosting_node(),

    %% Test that remsh works with explicit -sname.
    HostNode = atom_to_list(?FUNCTION_NAME) ++ "_host",
    HostNodeStr = printed_atom(list_to_atom(HostNode ++ "@" ++ Host)),
    rtnode(PreCmds ++
               [{putline,"nodes()."},
                {expect, "\\Q" ++ HostNodeStr ++ "\\E"}] ++
               PostCmds,
           HostNode, " ", "-remsh " ++ TargetNodeStr),

    %% Test that remsh works without -sname.
    rtnode(PreCmds ++ PostCmds, [], " ", "-remsh " ++ TargetNodeStr),

    peer:stop(Peer),

    ok.

quit_hosting_node() ->
    %% Command sequence for entering a shell on the hosting node.
    [{putdata, "\^g"},
     {expect, "--> $"},
     {putline, "s"},
     {expect, "--> $"},
     {putline, "c"},
     {expect, ["Eshell"]},
     {expect, ["1> $"]}].

%% Test that -remsh works with long names.
remsh_longnames(Config) when is_list(Config) ->
    %% If we cannot resolve the domain, we need to add localhost to the longname
    Domain =
        case inet_db:res_option(domain) of
            [] ->
                "@127.0.0.1";
            _ -> ""
        end,
    case rtstart(" -name " ++ atom_to_list(?FUNCTION_NAME)++Domain) of
        {ok, _SRPid, STPid, SState} ->
            try
                {ok, _CRPid, CTPid, CState} =
                    rtstart("-name undefined" ++ Domain ++
                                " -remsh " ++ atom_to_list(?FUNCTION_NAME)),
                try
                    ok = send_commands(
                           STPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
                    ok = send_commands(
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtnode_dump_logs(rtstop(CState))
                end
            after
                rtnode_dump_logs(rtstop(SState))
            end;
        Else ->
            Else
    end.

%% Test that -remsh works without epmd.
remsh_no_epmd(Config) when is_list(Config) ->
    EPMD_ARGS = "-start_epmd false -erl_epmd_port 12345 ",
    case rtstart([],"ERL_EPMD_PORT=12345 ",
                 EPMD_ARGS ++ " -sname " ++ atom_to_list(?FUNCTION_NAME)) of
        {ok, _SRPid, STPid, SState} ->
            try
                ok = send_commands(
                       STPid,
                       [{putline, ""},
                        {putline, "node()."},
                        {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
                {ok, _CRPid, CTPid, CState} =
                    rtstart([],"ERL_EPMD_PORT=12345 ",
                            EPMD_ARGS ++ " -remsh "++atom_to_list(?FUNCTION_NAME)),
                try
                    ok = send_commands(
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtstop(CState)
                end
            after
                rtstop(SState)
            end;
        Else ->
            Else
    end.

rtnode(C) ->
    rtnode(C, [], [], []).

rtnode(C, N) ->
    rtnode(C, N, [], []).

rtnode(Commands, Nodename, ErlPrefix) ->
    rtnode(Commands, Nodename, ErlPrefix, []).

rtnode(Commands, Nodename, ErlPrefix, Args) ->
    case rtstart(Nodename, ErlPrefix, Args) of
        {ok, _SPid, CPid, RTState} ->
            Res = catch send_commands(CPid, Commands, 1),
            Logs = rtstop(RTState),
            case Res of
                ok ->
                    rtnode_dump_logs(Logs),
                    ok;
                _ ->
                    rtnode_dump_logs(Logs),
                    ok = Res
            end,
            {ok, Logs};
        Skip ->
            Skip
    end.

rtstart(Args) ->
    rtstart([], " ", Args).

rtstart(Nodename, ErlPrefix, Args) ->
    case get_progs() of
	{error,_Reason} ->
	    {skip,"No runerl present"};
	{RunErl,ToErl,[Erl|ErlArgs] = ErlWArgs} ->
	    case create_tempdir() of
		{error, Reason2} ->
		    {skip, Reason2};
		Tempdir when ErlPrefix =/= [] ->
		    SPid =
			start_runerl_node(RunErl,
                                          ErlPrefix++"\\\""++Erl++"\\\" "++
                                              lists:join($\s, ErlArgs),
					  Tempdir,Nodename,Args),
		    CPid = start_toerl_server(ToErl,Tempdir,undefined),
                    {ok, SPid, CPid, {CPid, SPid, ToErl, Tempdir}};
                Tempdir ->
                    SPid = start_peer_runerl_node(RunErl,ErlWArgs,Tempdir,Nodename,Args),
                    CPid = start_toerl_server(ToErl,Tempdir,SPid),
                    {ok, SPid, CPid, {CPid, SPid, ToErl, Tempdir}}
            end
    end.

rtstop({CPid, SPid, ToErl, Tempdir}) ->
    %% Unlink from peer so that we don't crash when peer quits
    unlink(SPid),
    case stop_runerl_node(CPid) of
        {error,_} ->
            catch rtstop_try_harder(ToErl, Tempdir, SPid);
        _ ->
            ok
    end,
    wait_for_runerl_server(SPid),
    Logs = rtnode_read_logs(Tempdir),
    file:del_dir_r(Tempdir),
    Logs.

rtstop_try_harder(ToErl, Tempdir, SPid) ->
    CPid = start_toerl_server(ToErl, Tempdir, SPid),
    ok = send_commands(CPid,
                       [{putline,[7]},
                        {expect, " --> $"},
                        {putline, "s"},
                        {putline, "c"},
                        {putline, ""}], 1),
    stop_runerl_node(CPid).

timeout(longest) ->
    timeout(long) + timeout(normal);
timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().

send_commands(CPid, [{sleep, X}|T], N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    send_commands(CPid, T, N+1)
    end;
send_commands(CPid, [{expect, Expect}|T], N) when is_list(Expect) ->
    ?dbg(Exp),
    case command(CPid, {expect, [Expect], timeout(normal)}) of
        ok ->
            send_commands(CPid, T, N + 1);
        {expect_timeout, Got} ->
            ct:pal("expect timed out waiting for ~p\ngot: ~p\n", [Expect,Got]),
            {error, timeout};
        Other ->
            Other
    end;
send_commands(CPid, [{putline, Line}|T], N) ->
    send_commands(CPid, [{putdata, Line ++ "\n"}|T], N);
send_commands(CPid, [{putdata, Data}|T], N) ->
    ?dbg({putdata, Data}),
    case command(CPid, {send_data, Data}) of
        ok ->
	    send_commands(CPid, T, N+1);
        Error ->
            Error
    end;
send_commands(_CPid, [], _) ->
    ok.

command(Pid, Req) ->
    Timeout = timeout(longest),
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Req},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    after Timeout ->
            io:format("timeout while executing ~p\n", [Req]),
            {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid),
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, SPid, _Reason} ->
	    ok
    after Timeout ->
	    {error, runerl_server_timeout}
    end.

stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(longest),
    receive
	{'DOWN', Ref, process, CPid, noproc} ->
	    ok;
	{'DOWN', Ref, process, CPid, normal} ->
	    ok;
	{'DOWN', Ref, process, CPid, {error, Reason}} ->
	    {error, Reason}
    after Timeout ->
	    {error, toerl_server_timeout}
    end.

get_progs() ->
    case os:type() of
        {unix,freebsd} ->
            {error,"Can't use run_erl on FreeBSD"};
        {unix,openbsd} ->
            {error,"Can't use run_erl on OpenBSD"};
        {unix,_} ->
            RunErl = find_executable("run_erl"),
            ToErl = find_executable("to_erl"),
            Erl = string:split(ct:get_progname()," ",all),
            {RunErl, ToErl, Erl};
        _ ->
            {error,"Not a Unix OS"}
        end.

find_executable(Name) ->
    case os:find_executable(Name) of
        Prog when is_list(Prog) ->
            Prog;
        false ->
            throw("Could not find " ++ Name)
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
	     io_lib:format("Unable to create ~s, reason eexist",
			   [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    %% Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
	{error, eexist} ->
	    create_tempdir(Dir0, Ch+1);
	{error, Reason} ->
	    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason ~p",
				   [Dir,Reason])),
	    {error,Estr};
	ok ->
	    Dir
    end.

start_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    XArg = case Nodename of
	       [] ->
		   [];
	       _ ->
		   " -sname "++(if is_atom(Nodename) -> atom_to_list(Nodename);
				   true -> Nodename
				end)++
		       " -setcookie "++atom_to_list(erlang:get_cookie())
	   end ++ " " ++ Args,
    spawn(fun() -> start_runerl_command(RunErl, Tempdir, Erl++XArg) end).

start_runerl_command(RunErl, Tempdir, Cmd) ->
    FullCmd = "\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++" \""++Cmd++"\"",
    ct:pal("~ts",[FullCmd]),
    os:cmd(FullCmd).

start_peer_runerl_node(RunErl,Erl,Tempdir,[],Args) ->
    start_peer_runerl_node(RunErl,Erl,Tempdir,peer:random_name(),Args);
start_peer_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    {ok, Peer, _Node} =
        ?CT_PEER(#{ name => Nodename,
                    exec => {RunErl,Erl},
                    detached => false,
                    shutdown => 10000,
                    post_process_args =>
                        fun(As) ->
                                [Tempdir++"/",Tempdir,
                                 lists:flatten(
                                   lists:join(
                                     " ",[[$',A,$'] || A <- As]))]
                        end,
                    args => ["-connect_all","false"|Args] }),
    Peer.

start_toerl_server(ToErl,Tempdir,SPid) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir,SPid]),
    receive
	{Pid,started} ->
	    Pid;
	{Pid,error,Reason} ->
	    {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof]),
    Timeout = timeout(short) div 2,
    receive
	{Port, eof} ->
            timer:sleep(Timeout),
	    try_to_erl(Command, N-1)
    after Timeout ->
	    ?dbg(Port),
	    Port
    end.

toerl_server(Parent, ToErl, TempDir, SPid) ->
    Port = try_to_erl("\""++ToErl++"\" "++TempDir++"/ 2>/dev/null", 8),
    case Port of
	P when is_port(P) ->
	    Parent ! {self(),started};
	{error,Other} ->
	    Parent ! {self(),error,Other},
	    exit(Other)
    end,

    State = #{port => Port, acc => [], spid => SPid},
    case toerl_loop(State) of
	normal ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("toerl_server exit with reason ~p~n",
				   [Reason]),
	    exit(Reason)
    end.

toerl_loop(#{port := Port} = State0) ->
    ?dbg({toerl_loop, Port, map_get(acc, State0),
          maps:get(match, State0, nomatch)}),

    State = handle_expect(State0),

    receive
	{Port,{data,Data}} when is_port(Port) ->
	    ?dbg({?LINE,Port,{data,Data}}),
            toerl_loop(State#{acc => map_get(acc, State) ++ Data});
        {Pid, Ref, {expect, Expect, Timeout}} ->
            toerl_loop(init_expect(Pid, Ref, Expect, Timeout, State));
        {Pid, Ref, {send_data, Data}} ->
            Port ! {self(), {command, Data}},
	    Pid ! {Ref, ok},
	    toerl_loop(State);
	{_Pid, kill_emulator} ->
            kill_emulator(State);
        {timeout,Timer,expect_timeout} ->
            toerl_loop(handle_expect_timeout(Timer, State));
	{Port, eof} ->
	    {error, unexpected_eof};
	Other ->
	    {error, {unexpected, Other}}
    end.

kill_emulator(#{spid := SPid, port := Port}) when is_pid(SPid) ->
    catch peer:stop(SPid),
    wait_for_eof(Port);
kill_emulator(#{port := Port}) ->
    %% If the line happens to end in a ".", issuing "init:stop()."
    %% will result in a syntax error.  To avoid that, issue a "\n"
    %% before "init:stop().".
    Port ! {self(),{command, "\ninit:stop().\n"}},
    wait_for_eof(Port).

wait_for_eof(Port) ->
    receive
        {Port,eof} ->
            normal;
        _Other ->
            wait_for_eof(Port)
    after
        timeout(long) ->
            {error, kill_timeout}
    end.

init_expect(Pid, Ref, ExpectList, Timeout, State) ->
    try compile_expect(ExpectList) of
        Expect ->
            Exp = #{expect => Expect,
                    ref => Ref,
                    source => ExpectList,
                    timer => erlang:start_timer(Timeout, self(), expect_timeout),
                    from => Pid},
            State#{expect => Exp}
    catch
        Class:Reason:Stk ->
            io:put_chars("Compilation of expect pattern failed:"),
            io:format("~p\n", [ExpectList]),
            io:put_chars(erl_error:format_exception(Class, Reason, Stk)),
            exit(expect_pattern_error)
    end.

handle_expect(#{acc := Acc, expect := Exp} = State) ->
    #{expect := Expect, from := Pid, ref := Ref} = Exp,
    case Expect(Acc) of
        nomatch ->
            State;
        {matched, Eaten, Result} ->
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end;
handle_expect(State) ->
    State.

handle_expect_timeout(Timer, State) ->
    #{acc := Acc, expect := Exp} = State,
    #{expect := Expect, timer := Timer, from := Pid, ref := Ref} = Exp,
    case Expect({timeout, Acc}) of
        nomatch ->
            Result = {expect_timeout, Acc},
            Pid ! {Ref, Result},
            finish_expect(0, State);
        {matched, Eaten, Result} ->
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end.

finish_expect(Eaten, #{acc := Acc0,
                       expect := #{timer := Timer}}=State) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout,Timer,timeout} ->
            ok
    after 0 ->
            ok
    end,
    Acc = lists:nthtail(Eaten, Acc0),
    maps:remove(expect, State#{acc := Acc}).

compile_expect([{timeout,Action}|T]) when is_function(Action, 1) ->
    Next = compile_expect(T),
    fun({timeout, _}=Tm) ->
            {matched, 0, Action(Tm)};
       (Subject) ->
            Next(Subject)
    end;
compile_expect([{{re,RE0},Action}|T]) when is_binary(RE0), is_function(Action, 1) ->
    {ok, RE} = re:compile(RE0),
    Next = compile_expect(T),
    fun({timeout, _}=Subject) ->
            Next(Subject);
       (Subject) ->
            case re:run(Subject, RE, [{capture,first,index}]) of
                nomatch ->
                    Next(Subject);
                {match, [{Pos,Len}]} ->
                    Matched = binary:part(list_to_binary(Subject), Pos, Len),
                    {matched, Pos+Len, Action(Matched)}
            end
    end;
compile_expect([RE|T]) when is_list(RE) ->
    Ok = fun(_) -> ok end,
    compile_expect([{{re,list_to_binary(RE)},Ok}|T]);
compile_expect([]) ->
    fun(_) ->
            nomatch
    end.

rtnode_check_logs(Logname, Pattern, Logs) ->
rtnode_check_logs(Logname, Pattern, true, Logs).
rtnode_check_logs(Logname, Pattern, Match, Logs) ->
        case re:run(maps:get(Logname, Logs), Pattern) of
            {match, [_]} when Match ->
                ok;
            nomatch when not Match ->
                ok;
            _ ->
                rtnode_dump_logs(Logs),
                ct:fail("~p not found in log ~ts",[Pattern, Logname])
        end.

rtnode_dump_logs(Logs) ->
    maps:foreach(
      fun(File, Data) ->
              ct:pal("~ts: ~ts",[File, Data])
      end, Logs).

rtnode_read_logs(Tempdir) ->
    {ok, LogFiles0} = file:list_dir(Tempdir),

    %% Make sure that we only read log files and not any named pipes.
    LogFiles = [F || F <- LogFiles0,
                     case F of
                         "erlang.log" ++ _ -> true;
                         _ -> false
                     end],

    lists:foldl(
      fun(File, Acc) ->
              case file:read_file(filename:join(Tempdir, File)) of
                  {ok, Data} ->
                      Acc#{ File => Data };
                  _ ->
                      Acc
              end
      end, #{}, LogFiles).

get_default_shell() ->
    try
        rtnode([{putline,""},
                {putline, "is_pid(whereis(user_drv))."},
                {expect, "true\r\n"}]),
        new
    catch _E:_R ->
            ?dbg({_E,_R}),
            old
    end.

printed_atom(A) ->
    lists:flatten(io_lib:format("~w", [A])).
