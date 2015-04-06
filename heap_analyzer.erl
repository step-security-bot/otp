-module(heap_analyzer).

%% Description
%%
%% This module converts a raw erlang:process_info(Pid, memory_dump)
%% to a digraph representing the heap. Each vertex in the digraph
%% represents one memory location, except:
%%   * the 0 vertex which is used as a the root for all roots.
%%     i.e. digraph:out_neighbours(G, 0) gives all roots
%%   * constants, we do not have access to the values to constants
%%     so the only thing we know is the type.
%%
%% As an example lets say we have a small process with only one
%% conscell pointer to by a pointer on the stack.
%%
%%     0  ------> 4000 ------> 4100
%%                  \--------> 4108
%%
%% In the example above 0 points to the stack element which is on
%% address 4000. The stack element points to both the head and the
%% tail of the conscell.
%%
%% So if we wanted to get all roots, it is possible to do:
%%   digraph:out_neighbours(G, 0).
%% or if we want to get all live data:
%%   digraph_utils:reachable_neighbours([0], G).
%%
%% Each vertex also has some meta data attached to it as the label.
%% The data depends on the type of the data. For the example above the
%% stack vertex could look like this.
%%
%%   1> digraph:vertex(G, 4000).
%%   {4000,#{container => stack,label => stack,type => boxed}}
%% The container describes what type of term this term is part of,
%% the label is the memory area and the type if the term.
%%
%%   2> [digraph:vertex(G, V) || V <- digraph:out_neighbours(G,4000)].
%%   [{4100,#{container => cons,label => new_heap,payload => 1,type => small}},
%%    {4108,#{container => cons,label => new_heap,type => nil}}]
%%
%% In the example above we take the out neighbours of the cons ptr
%% (i.e. the cons cell) and get their meta data. Here we can see that
%% the container is cons, and the label is the new_heap. We can also see that
%% the type of the head is small and the value is 1. The tail of the list
%% is nil, i.e. the empty list.
%%
%% This module also contains some helpful analytics functions.
%%    heap_analyzer:analyze(G) returns overview stats of the allocated data
%%    heap_analyzer:addr_to_term(G, Addr) converts a given memory address
%%       to it's original erlang terms. If a constant is found, the atom
%%       '$CONST' will be used instead.

-export([parse/1,analyze/1]).

-export([test/0]).

test() ->
    P = self(),
    spawn_opt(fun() ->
		  M = maps:from_list([{I,I} || I <- lists:seq(1,64)]),
		  try
		  Terms = {0.0, make_ref(), self(),
			   #{ foo => bar },
			   fun(A) -> {A, M} end,
			   file:open("/tmp/test",[raw]),
			   1 bsl 80, lists:seq(1,100), M},
		  P ! erlang:process_info(self(),memory_dump),
		  receive inf -> Terms end
		  catch _E:_R ->
			  P ! M
		  end
	  end,[{min_heap_size,1000}]),
    receive
	M ->
	    M
    end.

parse(Pid) when is_pid(Pid) ->
    parse(erlang:process_info(Pid, memory_dump));
parse({memory_dump, Dump}) ->
    G = digraph:new([acyclic]),
    try
        add_vertex(G, 0, #{ type => root, label => root }),

	Meta = #{ addr2line => proplists:get_value(addr2line, Dump),
		  catch2line => proplists:get_value(catch2line, Dump) },

	Roots = proplists:get_value(roots, Dump), 

	[ parse_root(G, Root, RootsAddr, Label, Meta)
	  || {Label, RootsAddr, Root} <- Roots],

	Heaps = proplists:get_value(heaps, Dump),

	[ parse_heap(G, Heap, HeapAddr, Label, Meta)
	  || {Label, HeapAddr, Heap} <- Heaps],

	%% Add labels for constants
        [begin
	     case [Label || {Label, HeapAddr, Heap} <- Heaps ++ Roots,
			    (V >= HeapAddr) andalso
			    (V =< HeapAddr + (size(Heap)*8))] of
		 [] when V =/= 0 ->
		     {_,L} = digraph:vertex(G, V),
		     %% Assert that there is no label set
		     error = maps:find(label, L),
		     digraph:add_vertex(G, V, L#{ label => constant });
		 _ ->
		     ok
	     end
	 end || V <- digraph:vertices(G)],

	verify_graph(G),

	G
    catch E:R ->
	    {G, {E,R,erlang:get_stacktrace()}}
    end.

parse_root(G, <<I:64/integer-native,R/binary>>, Addr, Label,
	    #{addr2line := A2L } = Meta) 
  when (I band 16#3) == 0 ->
    Addr = add_vertex(G, Addr,#{ type => cp, label => Label,
				 payload => proplists:get_value(I, A2L) }),
    add_edge(G, 0, Addr, Label),
    parse_root(G, R, Addr + 8, Label, Meta);
parse_root(G, <<I:64/integer-native,R/binary>>, Addr, Label, Meta) ->
    add_vertex(G, Addr, parse_term(G, I, Addr, Label, Meta)),
    add_edge(G, 0, Addr, Label),
    parse_root(G, R, Addr + 8, Label, Meta);
parse_root(G, <<>>, _Addr, _Label, _Meta) ->
    G.

parse_heap(G, <<I:64/integer-native, R/binary>>, Addr, Label, Meta)
  when (I band 16#3F) =:= 0 ->
    %% Tuples
    Arity = (I bsr 6),
    add_vertex(G, Addr, #{ type => tuple, arity => Arity,
			   label => Label, container => tuple }),
    add_edges(G, Addr, Addr, Arity, tuple),
    parse_heap(G, R, Addr + 8, Label, Meta);
parse_heap(G, <<I:64/integer-native, Size:64/integer-native, R/binary>>,
	   Addr, Label, Meta)
  when (I band 16#FF) == 16#3C ->
    %% Flat maps
    add_vertex(G, Addr, #{ type => flat_map, size => Size,
			   label => Label, container => flat_map }),

    add_vertex(G, Addr + 8, #{ type => payload, label => Label }),
    add_edge(G, Addr, Addr + 8, flat_map),

    add_edges(G, Addr, Addr, Size + 1, flat_map),

    parse_heap(G, R, Addr + 16, Label, Meta);
parse_heap(G, <<I:64/integer-native, R/binary>>,
	   Addr, Label, Meta)
  when (I band 16#FF) == 16#7C ->
    %% Hash Map node
    BitMap = I bsr 16,
    Size = popcount(BitMap),
    add_vertex(G, Addr, #{ type => hashmap_node,
			   node_size => Size,
			   bitmap => BitMap,
			   label => Label,
			   container => hashmap_node}),
    add_edges(G, Addr, Addr, Size, hashmap_node),
    parse_heap(G, R, Addr + 8, Label, Meta);
parse_heap(G, <<I:64/integer-native, Size:64/integer-native, R/binary>>,
	   Addr, Label, Meta)
  when (I band 16#BF) == 16#BC ->
    %% Hash Map head array or bitmap node
    BitMap = I bsr 16,
    NodeSize = popcount(BitMap),
    add_vertex(G, Addr, #{ type => hashmap_head,
			   node_size => NodeSize,
			   size => Size,
			   bitmap => BitMap,
			   label => Label,
			   container => hashmap_head}),
    add_edges(G, Addr, Addr, NodeSize + 1, hashmap_head),
    add_vertex(G, Addr + 8, #{ type => payload, label => Label }),
    parse_heap(G, R, Addr + 16, Label, Meta);
parse_heap(G, <<I:64/integer-native, R/binary>>, Addr, Label, Meta)
  when (I band 16#3) == 0 ->
    Arity = (I bsr 6),
    PayloadSize = Arity * 8,
    <<Payload:(PayloadSize)/binary, R2/binary>> = R,
    T = case (I bsr 2) band 16#F of
	    16#1 ->
		#{ type => binary_aggregate, payload => Payload};
	    16#2 ->
		#{ type => pos_bignum, payload => Payload};
	    16#3 ->
		#{ type => neg_bignum, payload => Payload};
	    16#4 ->
		#{ type => reference, payload => Payload};
	    16#5 ->
		<<_FE:64, _Next:64, _NativeAddr:64, _Arity:64,
		  NumFree:64/integer-native>> = Payload,
		[add_edge(G, Addr, Addr + 40 + I2 * 8, 'fun')
		 || I2 <- lists:seq(1, 1 + NumFree)],
		#{ type => 'fun', payload => Payload};
	    16#6 ->
		#{ type => float, payload => Payload};
	    16#7 ->
		#{ type => export, payload => Payload};
	    16#8 ->
		#{ type => refc_binary, payload => Payload};
	    16#9 ->
		#{ type => heap_binary, payload => Payload};
	    16#A ->
		add_edge(G, Addr, Addr + Arity * 8 + 8, sub_binary),
		#{ type => sub_binary, payload => Payload};
	    16#B ->
		#{ type => not_used, payload => Payload};
	    16#C ->
		#{ type => external_pid, payload => Payload};
	    16#D ->
		#{ type => external_port, payload => Payload};
	    16#E ->
		#{ type => external_reference, payload => Payload}
	end,
    Addr = add_vertex(G, Addr, T#{ label => Label }),
    [begin
	 add_vertex(G, Addr + I2 * 8, #{ type => payload, label => Label }),
	 add_edge(G, Addr, Addr + I2 * 8, maps:get(type, T))
     end || I2 <- lists:seq(1, Arity)],
    parse_heap(G, R2, Addr + 8 + Arity * 8, Label, Meta);
parse_heap(G, <<I:64/integer-native,R/binary>>, Addr, Label, Meta) ->
    add_vertex(G, Addr, parse_term(G, I, Addr, Label, Meta)),
    parse_heap(G, R, Addr + 8, Label, Meta);
parse_heap(G, <<>>, _Addr, _Label, _Meta) ->
    G.

parse_term(G, I, Addr, Label, _Meta) when (I band 16#3) == 1 ->
    add_edges(G, Addr, I - 1 - 8, 2, cons),
    #{ type => cons, label => Label };
parse_term(G, I, Addr, Label, _Meta) when (I band 16#3) == 2 ->
    add_edge(G, Addr, I - 2, boxed),
    #{ type => boxed, label => Label };
parse_term(_G, I, _Addr, Label, #{ catch2line := C2L })
  when (I band 16#3) == 3 ->
    T = case (I bsr 2) band 16#3 of
	    0 -> #{ type => pid, payload => I bsr 4};
	    1 -> #{ type => port, payload => I bsr 4};
	    2 -> case (I bsr 4) band 16#3 of
		     0 -> #{ type => atom, payload => I bsr 6};
		     1 -> #{ type => 'catch',
			     payload => proplists:get_value(I bsr 6,C2L)};
		     2 -> #{ type => immed2_unused };
		     3 -> #{ type => nil }
		 end;
	    3 -> 
		Int = if 
			  I >= 16#80000000 ->
			      ((I band (bnot 16#80000000)) bsr 4) * -1;
			  true ->
			      I bsr 4
		      end,
		#{ type => small, payload => Int }
	end,
    T#{ label => Label }.

verify_graph(G) ->
    Vs = lists:sort(digraph:vertices(G)),

    %% First we verify that all expected entries are present
    lists:foldl(
      fun(Curr, Prev = {PV,#{ label := PLabel }} ) ->
	      V = {_,#{ label := CLabel }} = digraph:vertex(G, Curr),
	      if CLabel =/= PLabel ->
		      V;
		 CLabel =:= constant andalso PLabel =:= constant ->
		      V;
		 Curr =:= PV + 8 ->
		      V;
		 true ->
		      io:format("Graph verification failed:~n"),
		      print_vertex(G, PV),
		      io:format("~n"),
		      print_vertex(G, Curr),
		      io:format("~n"),
		      erlang:error({invalid_graph,Prev,V})
	      end
      end, digraph:vertex(G, hd(Vs)), tl(Vs)),

    %% Then we check that all boxed + cons ptrs point to expected
    %% nodes.
    lists:foreach(
      fun(Curr) ->
	      try
		  case digraph:vertex(G, Curr) of
		      {_, #{ type := cons } } ->
			  [CAR,CDR] = digraph:out_neighbours(G, Curr),
			  {_, #{ container := cons, type := CART,
				 label := CARLabel } }
			      = digraph:vertex(G, CAR),
			  true = lists:member(CART, non_boxed_terms())
			      orelse CARLabel =:= constant,
			  {_, #{ container := cons, type := CDRT,
				 label := CDRLabel } }
			      = digraph:vertex(G, CDR),
			  true = lists:member(CDRT, non_boxed_terms())
			      orelse CDRLabel =:= constant;
		      {_, #{ type := boxed } } ->
			  [Box] = digraph:out_neighbours(G, Curr),
		      {_, #{ type := BoxT, label := BoxLabel } }
			  = digraph:vertex(G, Box),
		      true = lists:member(BoxT, boxed_terms())
			      orelse BoxLabel =:= constant,
		      BoxedElements = digraph:out_neighbours(G, Box),
		      [] = [digraph:vertex(G, BE)
			    || BE <- BoxedElements,
			       BoxT =/= maps:get(container,
						 element(2,digraph:vertex(G, BE)))];
		      _ ->
			  ok
		  end
	      catch E:R ->
		      erlang:raise(error,{E,R,{faulty_ptr, Curr}},
				   erlang:get_stacktrace())
	      end
      end, Vs).


analyze(G) ->

    #{ new => analyze_heap(G, new_heap),
       stack => analyze_heap(G, stack),
       young => analyze_heap(G, young_heap),
       old => analyze_heap(G, old_heap)
     }.

analyze_heap(G, Label) ->

    NonConstG = digraph_utils:subgraph(G,digraph:vertices(G)),
    %% NonConstG = filtergraph(#{ label => constant }, G),

    LiveVs = digraph_utils:reachable_neighbours([0], NonConstG),
    LiveG = digraph_utils:subgraph(NonConstG, LiveVs),

    R = #{ live => analyze_heap_details(LiveG, #{ label => Label }),
	   all => analyze_heap_details(NonConstG, #{ label => Label }) },
    digraph:delete(NonConstG),
    digraph:delete(LiveG),
    R.


analyze_heap_details(G, Filtermap) ->
    #{ cons => length(filtervertices(Filtermap#{ container => cons }, G)),
       tuple => length(filtervertices(Filtermap#{ container => tuple }, G)),
       map => length(filtervertices(Filtermap#{ container => flat_map }, G)),
       hashmap => length(filtervertices(Filtermap#{ container => hashmap_node }, G))
       + length(filtervertices(Filtermap#{ container => hashmap_head }, G))
     }.

filtergraph(Filtermap, G) ->
    digraph_utils:subgraph(G, filtervertices(Filtermap, G)).

filtervertices(Filtermap, G) ->    
    lists:filter(fun(V) ->
			 {_, Label} = digraph:vertex(G, V),
			 lists:all(fun({Key, Value}) ->
					   case maps:find(Key, Label) of
					       {ok, Value} ->
						   true;
					       _ ->
						   false
					   end
				   end, maps:to_list(Filtermap))
		 end, digraph:vertices(G)).

popcount(0) ->
    0;
popcount(I) ->
    if
	I band 1 == 1 ->
	    1 + popcount(I bsr 1);
	true ->
	    popcount(I bsr 1)
    end.

add_edges(_G, _From, _To, 0, _Container) ->
    ok;
add_edges(G, From, To, ToSteps, Container) ->
    add_edge(G, From, To + ToSteps * 8, Container),
    add_edges(G, From, To, ToSteps-1, Container).
add_edge(G, From, To, Container) ->
    case digraph:vertex(G, To) of
	false ->
	    digraph:add_vertex(G, To, #{ type => unknown,
					 container => Container });
	{_,Label1} ->
	    digraph:add_vertex(G, To, Label1#{ container => Container })
    end,
    case digraph:vertex(G, From) of
	false ->
	    digraph:add_vertex(G, From, #{ type => unknown,
					   container => Container });
	_ ->
	    ok
    end,

    case digraph:add_edge(G, From, To) of
	{error,{bad_edge,Vertexes} = E} ->
	    io:format("Error adding edge 0x~.16B -> 0x~.16B because "
		      "cycle is formed inbetween:~n",[From,To]),
	    debug([print_with_neighbours(G, Vertex) || Vertex <- Vertexes]),
	    erlang:error(E);
	{error,Error} ->
	    io:format("Error adding edge ~p:~p ~p~n",[From,To,Error]),
	    erlang:error(Error);
	Edge ->
	    debug("Added 0x~.16B -> 0x~.16B~n",[From,To]),
	    Edge
    end.

add_vertex(G, Vertex, Label) ->
    
    %% Assert that we do not add the same vertex twice
    case digraph:vertex(G, Vertex) of
	false ->
	    debug("Added "),
	    L = Label;
	{Vertex,#{ type := unknown, container := C }} ->
	    debug("Updat "),
	    L = Label#{ container => C }
    end,

    case digraph:add_vertex(G, Vertex, L) of
	Vertex ->
	    debug("~s~n",[print_vertex(G, Vertex)]),
	    Vertex
    end.

print_in_neighbours(G, Vertex) ->
    [begin
	 [io_lib:format(" "),
	 print_vertex(G, InV),
	 io_lib:format(" ->~n")]
     end || InV <- digraph:in_neighbours(G, Vertex)].
print_out_neighbours(G, Vertex) ->
    [begin
	 [io_lib:format("     -> "),
	 print_vertex(G, OutV),
	 io_lib:format("~n")]
     end || OutV <- digraph:out_neighbours(G, Vertex)].

print_with_neighbours(G, Vertex) ->
    [print_in_neighbours(G, Vertex),
    io_lib:format("   "),
    print_vertex(G, Vertex),
    io_lib:format("~n"),
    print_out_neighbours(G, Vertex)].

print_vertex(G, Vertex) ->
    Label = case digraph:vertex(G, Vertex) of
		false ->
		    #{ };
		{Vertex,L} ->
		    L
	    end,
    [io_lib:format("0x~.16B: ",[Vertex]),
    io_lib:format("~p",[Label])].
%    io:format("~p ~p",[maps:get(type, Label, undefined),maps:get(label, Label, undefined)]).

debug(Fmt) ->
    debug(Fmt,[]).
debug(_Fmt,_Args) ->
%    io:format(_Fmt,_Args),
    ok.

non_boxed_terms() ->
    [cons,boxed,pid,port,atom,'catch',nil, small].

boxed_terms() ->
    [tuple,
     flat_map,
     hashmap_node,
     hashmap_head,
     binary_aggregate,
     pos_bignum,
     neg_bignum,
     reference,
     'fun',
     float,
     export,
     refc_binary,
     heap_binary,
     sub_binary,
     not_used,
     external_pid,
     external_port,
     external_reference].

all_terms() ->
    non_boxed_terms() ++ boxed_terms().
