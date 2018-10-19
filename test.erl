-module(test).

-export([test/0, go/1]).

test() ->
    T = ets:new(hej,[compressed]),
    Term = {hej,"hejhej",lists:duplicate(10,<<0:64>>)},
    ets:insert(T,Term),
    [Term] = ets:lookup(T,hej),
    ets:delete(T).

go(N) ->
    Data = ["akjshdkjsadh",list_to_tuple(lists:duplicate(10,"10")),
            maps:from_list([{I,I} || I <- lists:seq(1,5)])],
    Seq = lists:seq(1, N),

    T1 = ets:new(hej,[]),
    {UnCompressed, _} =
        timer:tc(fun() ->
                         [ets:insert(T1,{1,Data}) || _I <- Seq]
                 end),
    {UnCompressedLookup, _} =
        timer:tc(fun() ->
                         [ets:lookup(T1,{1,Data}) || _I <- Seq]
                 end),

    T2 = ets:new(hej,[compressed]),
%    msacc:start(),
    {Compressed, _} =
        timer:tc(fun() ->
                         [ets:insert(T2,{1,Data}) || _I <- Seq],
                         [ets:insert(T2,{1,Data}) || _I <- Seq],
                         [ets:insert(T2,{1,Data}) || _I <- Seq]
                 end),
%    msacc:stop(),msacc:print(),msacc:reset(),msacc:start(),
    {CompressedLookup, _} =
        timer:tc(fun() ->
                         [ets:lookup(T2,{1,Data}) || _I <- Seq],
                         [ets:lookup(T2,{1,Data}) || _I <- Seq],
                         [ets:lookup(T2,{1,Data}) || _I <- Seq]
                 end),
%    msacc:stop(),msacc:print(),
    ets:delete(T1),
    ets:delete(T2),
    #{insert => #{ uncompressed => UnCompressed,
                   compressed => Compressed,
                   ration => UnCompressed / Compressed * 100 },
      lookup => #{ uncompressed => UnCompressedLookup,
                   compressed => CompressedLookup,
                   ration => UnCompressedLookup / CompressedLookup * 100}
     }.
