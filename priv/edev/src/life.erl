%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 

%%
%% File:   life.erl
%% Author: Johan Montelius
%% Date:   2010-09-29
%%

-module(life).

-export([
	benchmark_arguments/0,
	benchmark_unit/0
    ]).


-export([start/3, time/3, start_cell/3]).

benchmark_arguments() ->
    [{time,[Rs,Cs,Gs]} || Rs <- [20,40], Cs <- [20,40], Gs <- [500,1000]].

benchmark_unit() ->
    "ms".

time(Rows, Cols, Gen) ->
    T1 = now(),
    start(Rows, Cols, Gen),
    T2 = now(), 
    timer:now_diff(T2,T1)/1000.

start(Rows, Cols, Gen) ->
    Toroid = create(Rows, Cols, self(), Gen),
    connect(Toroid),
    collect(Rows*Cols).


%% All cells send a message to the master when done.

collect(0)->
    ok;
collect(N) ->
    receive
	done ->
	    collect(N-1)
    end.


%% The toroid is repreneted by a list of lists but with the special
%% feature that a torid with rows 1..R is represented as
%% [R,1,2..,R,1]. This has the advantage that when connecting cells we
%% can take the first three rows and connect the second to the first
%% and third. Then we move down the list intil we connect R with R-1
%% and 1. This trick is then repeted for each row so a row of cells
%% 1..C is represented with the list [C,1,2...C,1]. :-)
   

create(Rows, Cols, Master, Gen) ->
    First = create_row(Cols, Master, Gen),
    Last = create_row(Cols, Master, Gen),    
    [First, Last | create_rows(Rows-2, Cols, [First, Last],  Master, Gen)].

create_rows(0, _, All, _Master, _Gen) ->
    All;
create_rows(Rows, Cols, All, Master, Gen) ->
    This = create_row(Cols, Master, Gen),    
    create_rows(Rows-1, Cols, [This|All], Master, Gen).    

create_row(Cols, Master, Gen) ->
    First = create_cell(Master, Gen),
    Last = create_cell(Master, Gen),
    [First, Last | create_row(Cols-2, [First, Last], Master, Gen)].

create_row(0, All, _Master, _Gen) ->
    All;
create_row(Cols, All, Master, Gen) ->
    This = create_cell(Master, Gen),
    create_row(Cols-1, [This|All], Master, Gen).

create_cell(Master, Gen) ->
    spawn(life, start_cell, [create_state(), Master, Gen]).

create_state() ->
    case random:uniform(2) of
	1 ->
	    dead;
	2 -> 
	    alive
    end.

%% Connecting cells now becomes easy. Run through all rows and send a
%% message to each cell with the process-ids of its neighbours.

connect([_,_]) ->
    ok;
connect([North, This, South | Rest]) ->
    connect_row(North, This, South),    
    connect([This, South | Rest]).

connect_row([_,_], [_,_], [_,_]) ->
    ok;
connect_row([NW, N, NE | North], [E, Cell, W | This], [SW, S, SE | South]) ->
    Cell !  {neighbors, [NW, N, NE, E, W, SW, S, SE]},
    connect_row([N, NE | North], [Cell, W |This], [S, SE | South]).

%% When a cell is created it does not know it's neighbours so it has
%% to wait for a message before it can proceed.

start_cell(State, Master, Gen) ->
    receive
	{neighbors, Neighbors} ->
	    life(Neighbors, self(), State, Master, Gen)
    end.

%% This is the implementation of the cell. It will send its state to
%% all its neighbours and then collect the states of its neighbours. A
%% new state is calculated and the next generation is executed.

life(_, _, _, Master, 0) ->
    Master ! done;
life(Neighbors, Me, State, Master, Gen) ->
    send_all(Neighbors, Me, State),
    Neighbour_states = receive_all(Neighbors),
    New_state = new_state(Neighbour_states, State),
    life(Neighbors, Me, New_state, Master, Gen-1).

%% All state messages are taged with the process id of the sender. A 
%% cell receives messages from all its neighbours one by one. 

receive_all(Neighbors) ->
    lists:map(fun(N) -> receive {N, State} -> State end end, Neighbors).

send_all(Neighbors, Me, State) ->
    lists:map(fun(N) -> N ! {Me, State} end, Neighbors).    

%% A new state is calculated when we know the next state of all our
%% neighbours. 

new_state(Neighbors, State) ->
    Alive = lists:foldr(fun(S,N) -> case S of alive -> N+1; dead -> N end end, 0, Neighbors),
    case Alive of
	0 -> dead;
	1 -> dead;
	2 -> State;
	3 -> alive;
        _ -> dead
    end.

				      
    
       


