%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

%% This is a quick prototype of the interface. We should look and see which sequences
%% are used by edlin + group + prim_tty to see if we need to expand some. For the
%% sequences that we can we should use prim_tty and terminfo to determine what to
%% output.
%%
%% https://hexdocs.pm/elixir/IO.ANSI.html
%% https://en.wikipedia.org/wiki/ANSI_escape_code
%% https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
%% https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
%%

-module(io_ansi).
-moduledoc """
""".

-export([format/1, format/2, format/3]).
-export([reset/0, ansi/1, sgr/1]).
-export([black/0, red/0, green/0, yellow/0, blue/0, magenta/0, cyan/0, white/0]).
-export([black_background/0, red_background/0, green_background/0, yellow_background/0,
         blue_background/0, magenta_background/0, cyan_background/0, white_background/0]).
-export([light_black/0, light_red/0, light_green/0, light_yellow/0, light_blue/0,
         light_magenta/0, light_cyan/0, light_white/0]).
-export([light_black_background/0, light_red_background/0, light_green_background/0,
         light_yellow_background/0, light_blue_background/0, light_magenta_background/0,
         light_cyan_background/0, light_white_background/0]).
-export([cursor/2, cursor_up/0, cursor_up/1, cursor_down/0, cursor_down/1,
         cursor_forward/0, cursor_forward/1, cursor_backward/0, cursor_backward/1,
         cursor_home/0, cursor_end/0]).

-type chardata() :: unicode:chardata() | [chardata() | code()].
-type format() :: io:format() | [string() | binary() | code()].

%% Windows VTS groups
%% -type cursor_position() :: ok.
%% -type cursor_visibility() :: ok.
%% -type cursor_shape() :: ok.
%% -type viewport_positioning() :: ok.
%% -type text_modification() :: ok.
%% -type text_formatting() :: ok.
%% -type screen_colors() :: ok.
%% -type mode_changes() :: ok.
%% -type query_state() :: ok.
%% -type tabs() :: ok.
%% -type designated_character_set() :: ok.
%% -type scrolling_margins() :: ok.
%% -type window_title() :: ok.
%% -type alternate_screen_buffer() :: ok.
%% -type window_width() :: ok.
%% -type soft_reset() :: ok.

-type foreground_color() :: black | blue | cyan | green | magenta | red | white | yellow |
                            {color, 0..255} | {color, R :: 0..5, G :: 0..5, B :: 0..5} |
                            light_foreground_color().
-type light_foreground_color() :: light_black | light_blue | light_cyan | light_green |
                                  light_magenta | light_red | light_white | light_yellow.
-type background_color() :: black_background | blue_background | cyan_background |
                            green_background | magenta_background | red_background |
                            white_background | yellow_background |
                            {color_background, 0..255} |
                            {color_background, R :: 0..5, G :: 0..5, B :: 0..5} |
                            light_background_color().
-type light_background_color() :: light_black_background | light_blue_background |
                                  light_cyan_background | light_green_background |
                                  light_magenta_background | light_red_background |
                                  light_white_background | light_yellow_background.
-type animation() :: blink_off | blink_rapid | blink_slow.
-type style() :: bright | conceal | crossed_out | encircled | encircled_framed_off |
                 faint | framed | inverse | inverse_off | italic | italic_off |
                 normal | overlined | overlined_off | reverse | reverse_off | underlined |
                 underlined_off.
-type font() :: primary_font | font1 | font2 | font3 | font4 | font5 | font6 | font7 | font8 | font9.
-type color() :: foreground_color() | background_color().

-type text_formatting() :: color() | style() | font().
-type text_modification() :: clear | clear_line | insert_character |
                             delete_character | erase_character |
                             insert_line.
-type text() :: animation() | text_formatting() | text_modification().
-type cursor() ::
        {cursor, Line :: non_neg_integer(), Column :: non_neg_integer()} |
        cursor_down | cursor_up | cursor_backward | cursor_forward |
        {cursor_down | cursor_backward | cursor_forward | cursor_up, N :: non_neg_integer()} |
        cursor_home | cursor_end.
-type code() :: text() | cursor().

-type option() :: { ansi, boolean()} | io_lib:options().
-type options() :: [option()].

-export_type([code/0, chardata/0]).

ansi(Arg) -> [$\e, Arg].

cursor(Line, Column) -> ["\e[",integer_to_list(Line),";",integer_to_list(Column),"H"].
cursor_up() -> "\e[A".
cursor_up(N) -> ["\e[",integer_to_list(N),"A"].
cursor_down() -> "\e[B".
cursor_down(N) -> ["\e[",integer_to_list(N),"B"].
cursor_forward() -> "\e[C".
cursor_forward(N) -> ["\e[",integer_to_list(N),"C"].
cursor_backward() -> "\e[D".
cursor_backward(N) -> ["\e[",integer_to_list(N),"D"].
cursor_home() -> "\e[0E". %% Elixir uses \e[H for this, i.e. coordinate 1,1 not X,1
cursor_end() -> "\e[F". %% Not sure if this works...

sgr([Arg]) -> ["\e[",Arg,"m"];
sgr(Args) -> ["\e[",lists:join($;,Args),"m"].
reset() -> sgr(["0"]).
black() -> sgr(["30"]).
red() -> sgr(["31"]).
green() -> sgr(["32"]).
yellow() -> sgr(["33"]).
blue() -> sgr(["34"]).
magenta() -> sgr(["35"]).
cyan() -> sgr(["36"]).
white() -> sgr(["37"]).

black_background() -> sgr(["40"]).
red_background() -> sgr(["41"]).
green_background() -> sgr(["42"]).
yellow_background() -> sgr(["43"]).
blue_background() -> sgr(["44"]).
magenta_background() -> sgr(["45"]).
cyan_background() -> sgr(["46"]).
white_background() -> sgr(["47"]).

light_black() -> sgr(["90"]).
light_red() -> sgr(["91"]).
light_green() -> sgr(["92"]).
light_yellow() -> sgr(["93"]).
light_magenta() -> sgr(["95"]).
light_blue() -> sgr(["94"]).
light_cyan() -> sgr(["96"]).
light_white() -> sgr(["97"]).

light_black_background() -> sgr(["100"]).
light_red_background() -> sgr(["101"]).
light_green_background() -> sgr(["102"]).
light_yellow_background() -> sgr(["103"]).
light_magenta_background() -> sgr(["105"]).
light_blue_background() -> sgr(["104"]).
light_cyan_background() -> sgr(["106"]).
light_white_background() -> sgr(["107"]).

-spec format(chardata()) -> unicode:chardata().
format(Format) ->
    format(Format, []).

-spec format(format(), Data :: [term()]) -> io_lib:chars().
format(Format, Data) ->
    format(Format, Data, []).

-spec format(format(), Data :: [term()], options()) -> io_lib:chars().
format(Format, Data, Options) ->
    UseAnsi = proplists:get_value(ansi, Options, true),
    AppendReset = [reset || UseAnsi andalso proplists:get_value(reset, Options, true)],
    try lists:foldl(
          fun(Ansi, {Acc, Args}) when is_atom(Ansi), UseAnsi ->
                  {[?MODULE:Ansi() | Acc], Args};
             (Ansi, {Acc, Args}) when is_tuple(Ansi), UseAnsi ->
                  {[apply(?MODULE, element(1, Ansi),
                         tuple_to_list(erlang:delete_element(1, Ansi))) | Acc], Args};
             (Ansi, {Acc, Args}) when is_atom(Ansi); is_tuple(Ansi) ->
                  {Acc, Args};
             (Fmt, {Acc, Args}) ->
                  {Scanned, Rest} = io_lib_format:scan(Fmt, Args),
                  {lists:reverse(Scanned) ++ Acc, Rest}
          end, {[], Data}, group(lists:flatten([Format,AppendReset]))) of
        {Scanned, []} ->
            io_lib_format:build(lists:reverse(Scanned));
        _ ->
            erlang:error(badarg, [Format, Data, Options])
    catch E:R:ST ->
            erlang:raise(E,R,ST)
%            erlang:error(badarg, [Format, Data, Options])
    end.

group(Fmt) ->
    group(Fmt, []).
group([Ansi | T], []) when is_atom(Ansi); is_tuple(Ansi) ->
    [Ansi | group(T, [])];
group([Ansi | T], Acc) when is_atom(Ansi); is_tuple(Ansi) ->
    [lists:reverse(Acc), Ansi | group(T, [])];
group([C | T], Acc) ->
    group(T, [C | Acc]);
group([], []) ->
    [];
group([], Acc) ->
    [lists:reverse(Acc)].
