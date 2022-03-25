%% coding: utf-8 -*-
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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
-module(prim_tty).
-compile(nowarn_export_all).
-compile(export_all).

%% Todo:
%%  * Try to move buffer handling logic to Erlang
%%    * This may not be possible for performance reasons, but should be tried
%%    * It seems like unix decodes and then encodes utf8 when emitting it
%%  * user_drv module should be able to handle both nif and driver without too many changes.
%%
%% Problems to solve:
%%  * Do not use non blocking io
%%  * Reset tty settings at _exit
%%  * Allow remsh in oldshell (can we do this?)
%%  * See if we can run a tty in windows shell
%%  * Allow unicode detection for noshell/noinput
%%  * ?Allow multi-line editing?
%%    * The current implementation only allows the cursor to move and edit on current line
%%
%% Concepts to keep in mind:
%%   Code point: A single unicode "thing", examples: "a", "😀" (unicode smilie)
%%   Grapheme cluster: One or more code points, "
%%   Logical character: Any character that the user typed or printed.
%%            One unicode grapheme cluster is a logical character
%%        Examples: "a", "\t", "😀" (unicode smilie), "\x{1F600}", "\e[0m" (ansi sequences),
%%                  "^C"
%%      When we step or delete we count logical characters even if they are multiple chars.
%%        (I'm unsure how ansi should be handled with regard to delete?)
%%
%%   Actual characters: The actual unicode grapheme clusters printed
%%   Column: The number of rendered columns for a logical character
%%
%%  When navigating using move(left) and move(right) the terminal will move one
%%  actual character, so if we want to move one logical character we may have to
%%  emit more moves. The same is true when overwriting.
%%
%%  When calculating the current column position we have to use the column size
%%  of the characters as otherwise smilies will becomes incorrect.
%%
%%  In the current ttysl_drv and also this implementation there are never any newlines
%%  in the buffer.
%%
%%  Printing of unicode characters:
%%    Read this post: https://jeffquast.com/post/terminal_wcwidth_solution/
%%    Summary: the wcwidth implementation in libc is often lacking. So we should
%%        create our own. We can get the size of all unicode graphemes by rendering
%%        them on a terminal and see how much the cursor moves. We can query where the
%%        cursor is by using "\033[6n"[1]. How many valid grapheme clusters are there?
%%
%%    On consoles that does support fetching the surrent cursor position, we may get
%%    away with only using that to check where we are and where to go. And on consoles
%%    that do not we just have to make a best effort and use libc wcwidth.
%%
%%    We need to know the width of characters when:
%%      * Printing a \t
%%      * Compensating for xn
%% [1]: https://www.linuxquestions.org/questions/programming-9/get-cursor-position-in-c-947833/
%%    Notes:
%%      - [129306,127996] (hand with skintone) seems to move the cursor more than
%%        it should on iTerm...The skintone code point seems to not
%%        work at all on Linux and instead emits hand + brown square which is 4 characters
%%        wide which is what the cursor on iTerm was positioned at. So maybe
%%        there is a mismatch somewhere in iTerm wcwidth handling.
%%      - edlin and user needs to agree on what one "character" is, right now edlin uses
%%        code points and not grapheme clusters.
%%      - We can deal with xn by always emitting it after a put_chars command. We must make
%%        sure not to emit it before we emit any newline in the put_chars though as that
%%        potentially will insert two newlines instead of one.
%%
%%  Windows:
%%    Since 2017:ish we can use Virtual Terminal Sequences[2] to control the terminal.
%%    It seems like these are mostly ANSI escape comparitible, so it should be possible
%%    to just use the same mechanism on windows and unix.
%% [2]: https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
%%    Windows does not seem to have any wcwidth, so maybe we have to generate a similar table
%%    as the PR in [3] and add string:width/1.
%% [3]: https://github.com/microsoft/terminal/pull/5795
%%
%%
%%  Things I've tried and discarded:
%%    * Use get position to figure out xn fix for newlines
%%      * There is too large a latency (about 10ms) to get the position, so things like
%%        `c:i()` becomes a lot slower.
%%    * Use tty insert mode
%%      * This only works when the cursor is on the last line, when it is on a previous
%%        line it only edit that line.
%%    * Use tty delete mode
%%      * Same problem as insert mode, it only deleted current line, and does not move
%%        to previous line automatically.

-export([init/1, window_size/1, unicode/1, unicode/2,
         putc_sync/2, putc/2, move/2, insert/2, delete/2,
         window_position/1, beep/1, on_load/0, on_load/1]).
-export_type([tty/0]).

-nifs([isatty/1, tty_init/2, tty_set/1, setlocale/0,
       tty_select/3, tty_window_size/1, write_nif/2, read_nif/2, isprint/1,
       wcwidth/1, wcswidth/1,
       sizeof_wchar/0, tgetent_nif/1, tgetnum_nif/1, tgetflag_nif/1, tgetstr_nif/1,
       tgoto_nif/2, tgoto_nif/3, tty_read_signal/2]).
-export([isprint/1,wcwidth/1, wcswidth/1, sizeof_wchar/0,
         tgetent/1, tgetnum/1, tgetflag/1, tgetstr/1, tgoto/2, tgoto/3]).

-on_load(on_load/0).

-record(state, {tty,
                signal,
                read,
                acc = <<>>,
                utf8, parent,
                buffer_before = [],  %% Current line before cursor in reverse
                buffer_after = [],   %% Current line after  cursor not in reverse
                cols = 80,
                xn = false,
                up = <<"\e[A">>,
                down = <<"\n">>,
                left = <<"\b">>,
                right = <<"\e[C">>,
                %% Tab to next 8 column windows is "\e[1I", for unix "ta" termcap
                tab = <<"\e[1I">>,
                insert = false,
                delete = false,
                position = <<"\e[6n">>, %% "u7" on my Linux
                position_reply = <<"\e\\[([0-9]+);([0-9]+)R">>,
                %% Copied from https://github.com/chalk/ansi-regex/blob/main/index.js
                ansi_regexp = <<"^[\e",194,155,"][[\\]()#;?]*(?:(?:(?:(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]+)*|[a-zA-Z\\d]+(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]*)*)?",7,")|(?:(?:\\d{1,4}(?:;\\d{0,4})*)?[\\dA-PR-TZcf-nq-uy=><~]))">>,
                %% The SGR (Select Graphic Rendition) parameters https://en.wikipedia.org/wiki/ANSI_escape_code#SGR
                ansi_sgr = <<"^[\e",194,155,"]\\[[0-9;:]*m">>
               }).

-spec on_load() -> ok.

on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: map().
on_load(Extra) ->
    case erlang:load_nif(atom_to_list(?MODULE), Extra) of
        ok -> ok;
        {error,{reload,_}} ->
            ok
    end.

-opaque tty() :: {pid(), reference()}.

-spec init([{canon | echo | sig, boolean()}]) -> {ok, tty()} | {error, term()}.
init(Options) ->
    ok = on_load(),
    Parent = self(),
    Ref = make_ref(),
    {Pid, _} = spawn_opt(
                    fun() -> init(Ref, Parent, Options) end,
                    [{monitor,[{tag,Ref}]}]),
    receive
        {Ref, _Ref, _, _, Reason} ->
            {error, Reason};
        {Ref, TTY} ->
            erlang:demonitor(Ref, [flush]),
            link(Pid),
            {ok, {Pid, TTY}}
    end.

window_size({_Pid, TTY}) ->
    tty_window_size(TTY).

window_position(TTY) ->
    call(TTY, get_position).

unicode(TTY) ->
    call(TTY, unicode).

unicode(TTY, Value) ->
    call(TTY, {unicode, Value}).

putc_sync(TTY, Characters) ->
    call(TTY, {putc, Characters}),
    self() ! {TTY, ok},
    ok.

putc(TTY, Characters) ->
    cast(TTY, {putc, Characters}).

move(TTY, Steps) ->
    cast(TTY, {move, Steps}).

insert(TTY, Characters) ->
    cast(TTY, {insert, Characters}).

delete(TTY, NumCharacters) ->
    cast(TTY, {delete, NumCharacters}).

beep(TTY) ->
    cast(TTY, beep).

init(Ref, Parent, Options) ->
    case {isatty(stdin), isatty(stdout)} of
        {true, true} ->
            State = init_tty(Options, os:type()),

            {ok, TraceFile} = file:open("tty.trace", [write]),
            dbg:tracer(
              process,
              {fun(Event, FD) ->
                    io:format(FD,"~tp~n", [Event]),
                    FD
              end, TraceFile}
            ),
            dbg:p(self(), [c, m, timestamp]),
        %    dbg:p(Parent, [c, r]),
            dbg:tpl(?MODULE, write_nif, []),
            dbg:tpl(?MODULE, read_nif, x),
            dbg:tpl(?MODULE, dbg, []),

            Utf8Mode =
                case setlocale() of
                    primitive ->
                        lists:any(
                            fun(Key) ->
                                string:find(os:getenv(Key,""),"UTF-8") =/= nomatch
                            end, ["LC_ALL", "LC_CTYPE", "LANG"]);
                    Utf8Locale when is_boolean(Utf8Locale) ->
                        Utf8Locale
                end,

            ok = tty_select(State#state.tty, State#state.signal, State#state.read),

        Parent ! {Ref, State#state.tty},
        try
            loop(update_cols(State#state{ utf8 = Utf8Mode, parent = Parent }))
        catch E:R:ST ->
            erlang:display({E,R,ST}),
            erlang:raise(E,R,ST)
        end;
    _ ->
        exit(notatty)
    end.

init_tty(Options, {unix,_}) ->
    {ok, TTY} = tty_init(stdout, maps:from_list(Options)),
    ok = tty_set(TTY),
    ok = tgetent(os:getenv("TERM")),

    %% See https://www.gnu.org/software/termutils/manual/termcap-1.3/html_mono/termcap.html#SEC23
    %% for a list of all possible termcap capabilities
    Cols = case tgetnum("co") of
               {ok, Cs} -> Cs;
               _ -> (#state{})#state.cols
           end,
    {ok, Up} = tgetstr("up"),
    Down = case tgetstr("do") of
               false -> (#state{})#state.down;
               {ok, D} -> D
           end,
    Left = case {tgetflag("bs"),tgetstr("bc")} of
               {true,_} -> (#state{})#state.left;
               {_,false} -> (#state{})#state.left;
               {_,{ok, L}} -> L
           end,
    {ok, Right} = tgetstr("nd"),

    Insert =
        case tgetstr("IC") of
            {ok, IC} -> IC;
            false -> (#state{})#state.insert
        end,

    Tab = case tgetstr("ta") of
              {ok, TA} -> TA;
              false -> (#state{})#state.tab
          end,

    Delete = case tgetstr("DC") of
                 {ok, DC} -> DC;
                 false -> (#state{})#state.delete
             end,

    Position = case tgetstr("u7") of
                   {ok, <<"\e[6n">> = U7} ->
                       %% User 7 should contain the codes for getting
                       %% cursor position.
                       % User 6 should contain how to parse the reply
                       {ok, <<"\e[%i%d;%dR">>} = tgetstr("u6"),
                       <<"\e[6n">> = U7, false;
                   false -> (#state{})#state.position
               end,

    #state{ tty = TTY,
            cols = Cols,
            xn = tgetflag("xn"),
            up = Up,
            down = Down,
            left = Left,
            right = Right,
            insert = Insert,
            delete = Delete,
            tab = Tab,
            position = Position,
            signal = make_ref(),
            read = make_ref()
    };
init_tty(Options, {win32, _}) ->
    {ok, TTY} = tty_init(stdout, maps:from_list(Options)),
    % os:putenv("ERL_CONSOLE_MODE","window"),
    #state{ tty = TTY,
            % position = false,
            xn = true,
            signal = make_ref(),
            read = make_ref() }.

loop(State) ->
    dbg({State#state.buffer_before, State#state.buffer_after}),
    receive
        {call, Ref, {unicode, true}} ->
            Ref ! {Ref, utf8},
            loop(State#state{ utf8 = true });
        {call, Ref, {unicode, false}} ->
            Ref ! {Ref, latin1},
            loop(State#state{ utf8 = false });
        {call, Ref, unicode} ->
            Ref ! {Ref, State#state.utf8 },
            loop(State);
        {call, Ref, get_position} ->
            Ref ! {Ref, get_position(State)},
            loop(State);
        {call, Ref, Request} ->
            dbg({request, Request}),
            NewState = handle_request(State, Request),
            Ref ! {Ref, ok},
            loop(NewState);
        {cast, Request} ->
            dbg({request, Request}),
            loop(handle_request(State, Request));
        {input, Bytes} ->
            State#state.parent ! {{self(), State#state.tty}, {data, Bytes}},
            loop(State);
        {select, TTY, Ref, ready_input}
          when TTY =:= State#state.tty,
               Ref =:= State#state.signal ->
            case tty_read_signal(TTY, Ref) of
                {ok, winch} ->
                    dbg(winch),
                    loop(update_cols(State));
                {ok, cont} ->
                    dbg(cont),
                    ok = tty_set(TTY),
                    loop(State)
            end;
        {select, TTY, Ref, ready_input}
          when TTY =:= State#state.tty,
               Ref =:= State#state.read ->
            {Bytes, Acc} = read_input(State),
            State#state.parent ! {{self(), State#state.tty}, {data, Bytes}},
            loop(State#state{ acc = Acc });
        {resize, {_Rows, Cols}} ->
            loop(State#state{ cols = Cols });
        _Unknown ->
            erlang:display({unknown, _Unknown}),
            loop(State)
    end.

read_input(State) ->
    {OS, _} = os:type(),
    CsToBin =
    case read(State) of
        {ok, Utf8Bytes} when is_binary(Utf8Bytes), OS =:= unix ->
            unicode:characters_to_binary([State#state.acc, Utf8Bytes]);
        {ok, UTF16NativeBytes} when is_binary(UTF16NativeBytes), OS =:= win32 ->
            unicode:characters_to_binary([State#state.acc, UTF16NativeBytes],
                {utf16, little}, utf8)
    end,
    case CsToBin of
        {error, B, _Error} ->
            {B, <<>>};
        {incomplete, B, Inc} ->
            {B, Inc};
        B when is_binary(B) ->
            {B, <<>>}
    end.

handle_request(State, {putc, Binary}) ->
    %% Todo should handle invalid utf8?
    insert_buf(State, Binary);
handle_request(State, {delete, N}) when N > 0 ->
    {DelNum, _DelCols, _, NewBA} = split(N, State#state.buffer_after),
    BBCols = cols(State#state.buffer_before),
    NewBACols = cols(NewBA),
    write(State,
          [NewBA, lists:duplicate(DelNum, $\s)]),
    write(State,
          [xnfix(State, BBCols + NewBACols + DelNum),
           move_cursor(State,
                       BBCols + NewBACols + DelNum,
                       BBCols)]),
    State#state{ buffer_after = NewBA };
handle_request(State, {delete, N}) when N < 0 ->
    {DelNum, DelCols, _, NewBB} = split(-N, State#state.buffer_before),
    dbg({delete, N, cols(State#state.buffer_before)}),
    NewBBCols = cols(NewBB),
    BACols = cols(State#state.buffer_after),
    write(State,
          [move_cursor(State, NewBBCols + DelCols, NewBBCols),
           State#state.buffer_after, lists:duplicate(DelCols, $\s)]),
    write(State,
          [xnfix(State, NewBBCols + BACols + DelNum),
           move_cursor(State, NewBBCols + BACols + DelCols, NewBBCols)]),
    State#state{ buffer_before = NewBB };
handle_request(State, {delete, 0}) ->
    State;
handle_request(State, {move, N}) when N < 0 ->
    {_DelNum, DelCols, NewBA, NewBB} = split(-N, State#state.buffer_before),
    Moves =
        case get_position(State) of
            {_Line, Col} when DelCols =< Col ->
                move(left, State, DelCols);
            _ ->
                NewBBCols = cols(NewBB),
                move_cursor(State, NewBBCols + DelCols, NewBBCols)
        end,
    write(State, Moves),
    State#state{ buffer_before = NewBB,
                 buffer_after = NewBA ++ State#state.buffer_after};
handle_request(State, {move, N}) when N > 0 ->
    {_DelNum, DelCols, NewBB, NewBA} = split(N, State#state.buffer_after),
    BBCols = cols(State#state.buffer_before),
    write(State, move_cursor(State, BBCols, BBCols + DelCols)),
    State#state{ buffer_after = NewBA,
                 buffer_before = NewBB ++ State#state.buffer_before};
handle_request(State, {move, 0}) ->
    State;
handle_request(State = #state{ xn = OrigXn }, {insert, Chars}) ->
    NewState0 = insert_buf(State#state{ xn = false }, Chars),
    NewState = NewState0#state{ xn = OrigXn },
    BBCols = cols(NewState#state.buffer_before),
    BACols = cols(NewState#state.buffer_after),
    write(NewState,NewState#state.buffer_after),
    write(NewState,[xnfix(State, BBCols + BACols),
                    move_cursor(State, BBCols + BACols, BBCols)]),
    NewState;
handle_request(State, beep) ->
    write(State, <<7>>),
    State;
handle_request(State, Req) ->
    erlang:display({unhandled_request, Req}),
    State.

%% Split the buffer after N logical characters returning
%% the number of real characters deleted and the column length
%% of those characters
split(N, Buff) ->
    split(N, Buff, [], 0, 0).
split(0, Buff, Acc, Chars, Cols) ->
    dbg({?FUNCTION_NAME, {Chars, Cols, Acc, Buff}}),
    {Chars, Cols, Acc, Buff};
split(N, _Buff, _Acc, _Chars, _Cols) when N < 0 ->
    ok = N;
split(_N, [], Acc, Chars, Cols) ->
    {Chars, Cols, Acc, []};
split(N, [Char | T], Acc, Cnt, Cols) when is_integer(Char) ->
    split(N - 1, T, [Char | Acc], Cnt + 1, Cols + npwcwidth(Char));
split(N, [Chars | T], Acc, Cnt, Cols) when is_list(Chars) ->
    split(N - length(Chars), T, [Chars | Acc], Cnt + length(Chars), Cols + cols(Chars));
split(N, [SkipChars | T], Acc, Cnt, Cols) when is_binary(SkipChars) ->
    split(N, T, [SkipChars | Acc], Cnt, Cols).

move_cursor(#state{ cols = W } = State, FromCol, ToCol) ->
    dbg({?FUNCTION_NAME, FromCol, ToCol}),
    [case (ToCol div W) - (FromCol div W) of
         0 -> "";
         N when N < 0 ->
             dbg({move, up, -N}),
             move(up, State, -N);
         N ->
             dbg({move, down, N}),
             move(down, State, N)
     end,
     case (ToCol rem W) - (FromCol rem W) of
         0 -> "";
         N when N < 0 ->
             dbg({down, left, -N}),
             move(left, State, -N);
         N ->
             dbg({down, right, N}),
             move(right, State, N)
     end].

move(up, #state{ up = Up }, N) ->
    lists:duplicate(N, Up);
move(down, #state{ down = Down }, N) ->
    lists:duplicate(N, Down);
move(left, #state{ left = Left }, N) ->
    lists:duplicate(N, Left);
move(right, #state{ right = Right }, N) ->
    lists:duplicate(N, Right).

cols([]) ->
    0;
cols([Char | T]) when is_integer(Char) ->
    npwcwidth(Char) + cols(T);
cols([Chars | T]) when is_list(Chars) ->
    cols(Chars) + cols(T);
cols([SkipSeq | T]) when is_binary(SkipSeq) ->
    %% Any binary should be an ANSI escape sequence
    %% so we skip that
    cols(T).

update_cols(State) ->
    case tty_window_size(State#state.tty) of
        {ok, {_Row, Cols}} when Cols > 0 ->
            dbg({?FUNCTION_NAME, Cols}),
            State#state{ cols = Cols };
        _Error ->
            dbg({?FUNCTION_NAME, _Error}),
            State
    end.

npwcwidth(Char) ->
    case wcwidth(Char) of
        {error, not_printable} -> 0;
        {error, enotsup} ->
            case unicode_util:is_wide(Char) of
                true -> 2;
                false -> 1
            end;
        C -> C
    end.

%% Return the xn fix for the current cursor position.
%% We use get_position to figure out if we need to calculate the current columns
%%  or not.
%%
%% We need to know the actual column because get_position will return the last
%% column number when the cursor is:
%%   * in the last column
%%   * off screen
%%
%% and it is when the cursor is off screen that we should do the xnfix.
xnfix(#state{ position = false } = State) ->
    xnfix(State, cols(State#state.buffer_before));
xnfix(State) ->
    {_Line, Col} = get_position(State),
    if Col =:= State#state.cols ->
            xnfix(State, cols(State#state.buffer_before));
       true ->
            []
    end.

%% Return the xn fix for CurrCols location.
xnfix(#state{ xn = true, cols = Cols } = State, CurrCols)
  when CurrCols rem Cols  == 0 ->
    [<<"\s">>,move(left, State, 1)];
xnfix(_, _CurrCols) ->
    dbg({xnfix, _CurrCols}),
    [].

insert_buf(State, Binary) when is_binary(Binary) ->
    insert_buf(State, Binary, []).
insert_buf(State, Bin, Acc) ->
    case string:next_grapheme(Bin) of
        [] ->
            NewBB = Acc ++ State#state.buffer_before,
            NewState = State#state{ buffer_before = NewBB },
            write(State, lists:reverse(Acc)),
            write(State, xnfix(NewState)),
            NewState;
        [$\t | Rest] ->
            insert_buf(State, Rest, [State#state.tab | Acc]);
        [$\e | Rest] ->
            case re:run(Bin, State#state.ansi_regexp, [unicode]) of
                {match, [{0, N}]} ->
                    <<Ansi:N/binary, AnsiRest/binary>> = Bin,
                    case re:run(Bin, State#state.ansi_sgr, [unicode]) of
                        {match, [{0, N}]} ->
                            %% We include the graphics ansi sequences in the
                            %% buffer that we step over
                            insert_buf(State, AnsiRest, [Ansi | Acc]);
                        _ ->
                            %% Any other ansi sequences are just printed and
                            %% then dropped as they "should" not effect rendering
                            write(State, Ansi),
                            insert_buf(State, AnsiRest, Acc)
                    end;
                _ ->
                    insert_buf(State, Rest, [$\e | Acc])
            end;
        [NLCR | Rest] when NLCR =:= $\n; NLCR =:= $\r ->
            Tail =
                if NLCR =:= $\n ->
                        <<$\r,$\n>>;
                   true ->
                        <<$\r>>
                end,
            [] = write(State, [lists:reverse(Acc), Tail]),
            insert_buf(State#state{ buffer_before = [] }, Rest, []);
        [Cluster | Rest] when is_list(Cluster) ->
            insert_buf(State, Rest, [Cluster | Acc]);
        %% We have gotten a code point that may be part of the previous grapheme cluster. 
        [Char | Rest] when Char >= 128, Acc =:= [], State#state.buffer_before =/= [] ->
            [PrevChar | BB] = State#state.buffer_before,
            case string:next_grapheme([PrevChar | Bin]) of
                [PrevChar | _] ->
                    %% It was not part of the previous cluster, so just insert
                    %% it as a normal character
                    insert_buf(State, Rest, [Char | Acc]);
                [Cluster | ClusterRest] ->
                    %% It was part of the previous grapheme cluster, so we output
                    %% it and insert it into the before_buffer
                    {_, ToWrite} = lists:split(length(lists:flatten([PrevChar])), Cluster),
                    write(State, ToWrite),
                    insert_buf(State#state{ buffer_before = [Cluster | BB] }, ClusterRest, Acc)
            end;
        [Char | Rest] when Char >= 128 ->
            insert_buf(State, Rest, [Char | Acc]);
        [Char | Rest] ->
            case {isprint(Char), Char} of
                {true,_} ->
                    insert_buf(State, Rest, [Char | Acc]);
                {false, 8#177} -> %% DEL
                    insert_buf(State, Rest, ["^?" | Acc]);
                {false, _} ->
                    insert_buf(State, Rest, ["^" ++ [Char bor 8#40] | Acc])
            end
    end.

%% Using get_position adds about 10ms of latency
get_position(#state{ position = false }) ->
    unknown;
get_position(State) ->
    [] = write(State, State#state.position),
    get_position(State, <<>>).
get_position(State, Acc) ->
    receive
        {select,TTY,Ref,ready_input}
            when TTY =:= State#state.tty,
                 Ref =:= State#state.read ->
            {Bytes, Acc} = read_input(State#state{ acc = Acc }),
            case re:run(Bytes, State#state.position_reply, [unicode]) of
                {match,[{Start,Length},Row,Col]} ->
                    <<Before:Start/binary,_:Length/binary,After/binary>> = Bytes,
                    %% This should be put in State in order to not screw up the
                    %% message order...
                    [State#state.parent ! {{self(), State#state.tty}, {data, <<Before/binary, After/binary>>}}
                     || Before =/= <<>>, After =/= <<>>],
                    {binary_to_integer(binary:part(Bytes,Row)),
                     binary_to_integer(binary:part(Bytes,Col))};
                nomatch ->
                    get_position(State, Bytes)
            end
    after 1000 ->
            unknown
    end.

call({Pid, _TTY}, Request) ->
    Mref = monitor(process, Pid, [{alias, demonitor}]),
    Pid ! {call, Mref, Request},
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, Reason} ->
            {error, Reason}
    end.

cast({Pid, _TTY}, Request) ->
    Pid ! {cast, Request},
    ok.

dbg(_) ->
    ok.

%% Nif functions
isatty(_Fd) ->
    erlang:nif_error(undef).
tty_init(_Fd, _Options) ->
    erlang:nif_error(undef).
tty_set(_TTY) ->
    erlang:nif_error(undef).
setlocale() ->
    erlang:nif_error(undef).
tty_select(_TTY, _SignalRef, _ReadRef) ->
    erlang:nif_error(undef).
write(_State, <<>>) ->
    [];
write(State, UTF8Binary) when is_binary(UTF8Binary) ->
    write_nif(State#state.tty, [UTF8Binary]);
write(State, Characters) ->
    write(State, unicode:characters_to_binary(Characters)).
write_nif(_TTY, _IOVec) ->
    erlang:nif_error(undef).
read(State) ->
    read_nif(State#state.tty, State#state.read).
read_nif(_TTY, _Ref) ->
    erlang:nif_error(undef).
tty_window_size(_TTY) ->
    erlang:nif_error(undef).
isprint(_Char) ->
    erlang:nif_error(undef).
wcwidth(_Char) ->
    erlang:nif_error(undef).
sizeof_wchar() ->
    erlang:nif_error(undef).
wcswidth(_Char) ->
    erlang:nif_error(undef).
tgetent(Char) ->
    tgetent_nif([Char,0]).
tgetnum(Char) ->
    tgetnum_nif([Char,0]).
tgetflag(Char) ->
    tgetflag_nif([Char,0]).
tgetstr(Char) ->
    tgetstr_nif([Char,0]).
tgoto(Char, Arg) ->
    tgoto_nif([Char,0], Arg).
tgoto(Char, Arg1, Arg2) ->
    tgoto_nif([Char,0], Arg1, Arg2).
tgetent_nif(_Char) ->
    erlang:nif_error(undef).
tgetnum_nif(_Char) ->
    erlang:nif_error(undef).
tgetflag_nif(_Char) ->
    erlang:nif_error(undef).
tgetstr_nif(_Char) ->
    erlang:nif_error(undef).
tgoto_nif(_Ent, _Arg) ->
    erlang:nif_error(undef).
tgoto_nif(_Ent, _Arg1, _Arg2) ->
    erlang:nif_error(undef).
tty_read_signal(_TTY, _Ref) ->
    erlang:nif_error(undef).

