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

-module(io_ansi).
-moduledoc """
""".

-export([format/1, format/2, format/3]).

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
        cursor_down | cursor_left | cursor_right | cursor_up |
        {cursor_down | cursor_left | cursor_right | cursor_up, N :: non_neg_integer()} |
        cursor_home | cursor_end.
-type code() :: text() | cursor().

-type option() :: { ansi, boolean()} | io_lib:options().
-type options() :: [option()].

-export_type([code/0, chardata/0]).

-spec format(chardata()) -> unicode:chardata().
format(Format) ->
    Format.

-spec format(format(), Data :: [term()]) -> io_lib:chars().
format(Format, Data) ->
    io_lib:format(Format, Data).

-spec format(format(), Data :: [term()], options()) -> io_lib:chars().
format(Format, Data, Options) ->
    io_lib:format(Format, Data, Options).
