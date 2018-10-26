-module(test).

%% Benchmark notes on 500000 values:
%% Uncompressed:
%%   - insert   776992 ms
%%   - lookup   395600 ms
%% Compressed BERT
%%   - insert  1882535 ms ( 142% slower )
%%   - lookup  3375164 ms ( 753% slower )
%%   - comp ratio is 3136 to 967 aka 30%
%% Compressed zstd ets dict:
%%   - insert  5497785 ms ( 607% slower )
%%   - lookup  1308252 ms ( 230% slower )
%%   - comp ratio is 3120 to 624 aka 20%
%% Compressed lz4:
%%   - insert  3116638 ms ( 300% slower )
%%   - lookup   795074 ms ( 100% slower )
%%   - comp ratio is 3120 to 1392 aka 44%

-export([test/0, go/1]).

test() ->
    T = ets:new(hej,[compressed]),
    Term = {hej,"hejhej",lists:duplicate(10,<<0:64>>)},
    ets:insert(T,Term),
    [Term] = ets:lookup(T,hej),
    ets:delete(T).

go(N) ->
    Seq = lists:seq(1, N),

    T1 = ets:new(hej,[public]),
    msacc:start(),
    UnCompressed = test(fun() ->
                                [ets:insert(T1,{1,data()}) || _I <- Seq]
                        end),
    msacc:stop(),msacc:print(msacc:stats(type, msacc:stats())),msacc:reset(),msacc:start(),
    UnCompressedLookup = test(fun() ->
                                      [ets:lookup(T1,1) || _I <- Seq]
                              end),

    msacc:stop(),msacc:print(msacc:stats(type, msacc:stats())),msacc:reset(),
    T2 = ets:new(hej,[public,compressed]),
    msacc:start(),

    Compressed = test(fun() ->
                              [ets:insert(T2,{1,data()}) || _I <- Seq]
                      end),
    msacc:stop(),msacc:print(msacc:stats(type, msacc:stats())),msacc:reset(),msacc:start(),
    CompressedLookup = test(fun() ->
                                    [ets:lookup(T2,1) || _I <- Seq]
                            end),
    msacc:stop(),msacc:print(msacc:stats(type, msacc:stats())),
    ets:delete(T1),
    ets:delete(T2),
    #{insert => #{ uncompressed => UnCompressed,
                   compressed => Compressed,
                   z_ratio => UnCompressed / Compressed * 100 },
      lookup => #{ uncompressed => UnCompressedLookup,
                   compressed => CompressedLookup,
                   z_ratio => UnCompressedLookup / CompressedLookup * 100}
     }.

test(Fun) ->
    spawn_opt(fun() -> {T,_} = timer:tc(Fun),exit(T) end,[monitor,{min_heap_size,512*1024*1024}]),
    receive
        {'DOWN',_,_,_,R} -> R
    end.

data() ->
    {128783909,
     [{{mvrsdm,1},
       registered,data_confirmed_by_hlr,location_confirmed_in_hlr,
       {'VS.MM.','NbrVisitingForeign.E'},
       undefined,
       {poolOngoing_state,false,false,undefined},
       false},
      {{monte_context,1},[]},
      {{mvria,1},"240993",{1540,190241,962160}},
      {{mvrdb,1},
       <<66,144,57,0,25,87,9,240>>,
       undefined,
       {<<"hss14">>,<<"ericsson.se">>},
       {mvrT_eSubscriptionData,
        <<33,19,32>>, 
        <<145,100,19,0,0,25,87,9,240>>,
        undefined_msisdn,"n*",
        {mvsgT_ambr,11000000,11000000},
        1,undefined,
        [{mvrT_apnSubscrRecord,1,
          [<<"apn1306-1">>,<<"ericsson">>,<<"com">>],
          ipv4,<<>>,undefined,
          {mvsgT_qualServ,eps,
           {mvsgT_epsQualityOfService,9, 
            {mvsgT_evolvedARP,15,false,true},
            undefined,undefined,undefined,undefined}},
          false,"n*",
          {dynamic,
           [<<"topon">>,<<"1302">>,<<"NAPTRa">>,<<"gtp">>,<<"pgw">>,
            <<"eth1">>,<<"gw1">>,<<"gbg1">>,<<"net">>,<<"epc">>,
            <<"mnc021">>,<<"mcc123">>,<<"3gppnetwork">>,<<"org">>]},
          {mvsgT_ambr,1000,1000},
          undefined,undefined,undefined,undefined,undefined}],
        " ",false,both_msc_and_sgsn,[],undefined,undefined,
        {mvsT_stnSr,<<145,33,67,101>>},
        true,undefined,undefined,[],undefined,[],undefined,[],
        consent_not_given,undefined,[],undefined,undefined,[],
        emergency_continuity_unsupported,[],undefined},
       {imsins_data,home,
        [<<"mnc021">>,<<"mcc123">>,<<"gprs">>],
        [],undefined,undefined,undefined,"ericsson.se",
        <<"ericsson.se">>,undefined,undefined,undefined,gtp,
        undefined,undefined,undefined,undefined,1,14,undefined,
        undefined,1,1,<<>>,undefined,s6d,true,undefined,true,
        undefined,true,undefined,undefined,undefined,undefined,1,2,
        true,0,true,allowed,[],undefined,undefined,noaction,
        undefined,allowed_cs_service,[],none,true,no_uut,denied,
        undefined,undefined,
        {mvsgT_plmnRateControl,0,0},
        undefined,undefined,undefined,undefined},
       [],
       <<131,100,0,5,102,97,108,115,101>>}]}.
