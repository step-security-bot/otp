%%
%% %CopyrightBegin%
%%                                                                                                                                   
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.                                                                             
%% 
%% The contents of this file are subject to the Erlang Public License,                                                               
%% Version 1.1, (the "License"); you may not use this file except in                                                                 
%% compliance with the License. You should have received a copy of the                                                               
%% Erlang Public License along with this software. If not, it can be                                                                 
%% retrieved online at http://www.erlang.org/.                                                                                       
%% 
%% Software distributed under the License is distributed on an "AS IS"                                                               
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See                                                               
%% the License for the specific language governing rights and limitations                                                            
%% under the License.                                                                                                                
%% 
%% %CopyrightEnd%

%% Author  : Björn-Egil Dahlberg
%% Created : 17 Dec 2008 by Björn-Egil Dahlberg

-module(renderspeed).

-export([opaque/2, transparent/2]).
-export([benchmark_arguments/0, benchmark_unit/0]).

benchmark_arguments() ->
    [{Engine, [W, H]} || Engine <- [opaque, transparent], W <- [500, 1000], H <- [500, 1000]].

benchmark_unit() -> "ms".

opaque(W, H) -> do(opaque, W,H).

transparent(W, H) -> do(alpha, W,H).

do(Engine, W, H) ->
    T0 = now(),
    Im = egd:create(W,H),
    make_rectangles(Im, W, H),
    make_ellipses(Im, W, H),
    make_lines(Im, W, H),
    _B = egd:render(Im, png, [{render_engine, Engine}]),
    %egd:save(B, "renderspeed_" ++ atom_to_list(Engine) ++ ".png"),
    egd:destroy(Im),
    T1 = now(),
    timer:now_diff(T1, T0)/1000.
    
make_lines(Im, W, H) ->
    Pts = [ {{X, 0}, {W - 1 - X, H - 1}} || X <- lists:seq(0, W - 1, 5) ],
    lists:map(
	fun({PtU,PtL}) ->
	    egd:line(Im, PtU, PtL, color())
	end, Pts). 

make_rectangles(Im, W, H) ->
    [ egd:filledRectangle(Im, {X,Y}, {X+10,Y+10}, color()) ||
	X <- lists:seq(0, W, 16),
	Y <- lists:seq(0, H, 16)
    ],
    ok.

make_ellipses(Im, W, H) ->
    [ 	egd:filledEllipse(Im, {X,Y}, {X+16,Y+16}, color()) ||
	X <- lists:seq(0, W, 8),
	Y <- lists:seq(0, H, 8)
    ],
    ok.

color() -> egd:color({byte(), byte(), byte(), 127}).

byte()  -> random:uniform(256) - 1.
