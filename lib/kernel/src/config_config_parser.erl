%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
-module(config_config_parser).

%% External exports
-export([parse/2, check_conf_sys/1]).

parse(Data, FName) ->
    BFName = filename:basename(FName),
    case parse_file_data(iolist_to_binary(Data), FName) of
        {ok, NewEnv} ->
            %% OTP-4867
            %% sys.config may now contain names of
            %% other .config files as well as
            %% configuration parameters.
            %% Therefore read and merge contents.
            if
                BFName =:= "sys" ->
                    DName = filename:dirname(FName),
                    {ok, SysEnv, Errors} =
                        check_conf_sys(NewEnv, [], [], DName),

                    %% Report first error, if any, and
                    %% terminate
                    %% (backwards compatible behaviour)
                    case Errors of
                        [] ->
                            {ok,SysEnv};
                        [{error, {SysFName, Line, Str}}|_] ->
                            throw({error, {SysFName, Line, Str}})
                    end;
                true ->
                    {ok, NewEnv}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check_conf_sys(Env) ->
    check_conf_sys(Env, [], [], []).

check_conf_sys([File|T], SysEnv, Errors, DName) when is_list(File),is_list(DName) ->
    BFName = filename:basename(File, ".config"),
    FName = filename:join(filename:dirname(File), BFName ++ ".config"),
    LName = case filename:pathtype(FName) of
               relative when (DName =/= []) ->
                  % Check if relative to sys.config dir otherwise use legacy mode,
                  % i.e relative to cwd.
                  RName = filename:join(DName, FName),
                  case erl_prim_loader:read_file_info(RName) of
                     {ok, _} -> RName ;
                     error   -> FName
                  end;
		_          -> FName
	    end,
    case load_file(LName) of
	{ok, NewEnv} ->
	    check_conf_sys(T, merge_env(SysEnv, NewEnv), Errors, DName);
	{error, {Line, _Mod, Str}} ->
	    check_conf_sys(T, SysEnv, [{error, {LName, Line, Str}}|Errors], DName)
    end;
check_conf_sys([Tuple|T], SysEnv, Errors, DName) ->
    check_conf_sys(T, merge_env(SysEnv, [Tuple]), Errors, DName);
check_conf_sys([], SysEnv, Errors, _) ->
    {ok, SysEnv, lists:reverse(Errors)}.

load_file(File) ->
    %% We can't use file:consult/1 here. Too bad.
    case erl_prim_loader:get_file(File) of
	{ok, Bin, _FileName} ->
            parse_file_data(Bin, File);
	error ->
	    {error, {none, open_file, "configuration file not found"}}
    end.

parse_file_data(Bin, FName) ->
    %% Make sure that there is some whitespace at the end of the string
    %% (so that reading a file with no NL following the "." will work).
    case file_binary_to_list(Bin) of
        {ok, String} ->
            scan_file(String ++ " ", FName);
        error ->
            {error, {none, scan_file, "bad encoding"}}
    end.

file_binary_to_list(Bin) ->
    Enc = case epp:read_encoding_from_binary(Bin) of
              none -> epp:default_encoding();
              Encoding -> Encoding
          end,
    case catch unicode:characters_to_list(Bin, Enc) of
        String when is_list(String) ->
            {ok, String};
        _ ->
            error
    end.

scan_file(Str, FName) ->
    case erl_scan:tokens([], "f() -> \n" ++ Str, 0) of
	{done, {ok, Tokens, _}, Left} ->
            AnnoTokens = [setelement(2,Tup,erl_anno:set_file(FName,element(2,Tup)))
                          || Tup <- Tokens],
	    case erl_parse:parse(AnnoTokens) of
		{ok,{function,_,f,_,[{clause,_,_,_,[L]}]}} ->
		    case only_ws(Left) of
			true ->
                            Normalised = normalise(L),
			    {ok, annolise(L, Normalised)};
			false ->
			    %% There was trailing garbage found after the list.
			    config_error()
		    end;
		{ok,_} ->
		    %% Parsing succeeded but the result is not a list.
		    config_error();
		Error ->
		    Error
	    end;
	{done, Result, _} ->
	    {error, {none, parse_file, tuple_to_list(Result)}};
	{more, _} ->
	    {error, {none, load_file, "no ending <dot> found"}}
    end.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
normalise({map,_,Pairs}=M) ->
    maps:from_list(
      lists:map(fun
		%% only allow '=>'
		({map_field_assoc,_,K,V}) -> {normalise(K),normalise(V)};
		(_) -> erlang:error({badarg,M})
	    end, Pairs));
normalise({'fun',_,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}) ->
    fun M:F/A;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

annolise({cons,Anno,Head,Tail}, Normal) ->
    case lists:all(
           fun(KV) ->
                   is_tuple(KV) andalso
                       tuple_size(KV) =:= 2
           end, Normal) of
        true ->
            [annolise(Head, hd(Normal))|annolise(Tail, tl(Normal))];
        false ->
            Anno
    end;
annolise({tuple,Anno,[AnnoKey,AnnoValue]}, {Key, Value}) ->
    Key = normalise(AnnoKey),
    {{Key,Anno},annolise(AnnoValue, Value)};
annolise({map,_,Pairs}, Normal) ->
    maps:from_list(
      lists:map(
        fun({{Key,Value},{map_field_assoc,Anno,AnnoKey,AnnoValue}}) ->
                Key = normalise(AnnoKey),
                {{Key,Anno},annolise(AnnoValue, Value)}
        end, lists:zip(
               lists:sort(maps:to_list(Normal)),
               lists:sort(
                 fun({_,_,K1,V1},{_,_,K2,V2}) ->
                         {normalise(K1),normalise(V1)} =< {normalise(K2),normalise(V2)}
                 end,Pairs))));
annolise({nil,_},_) -> [];
annolise(X, V) ->
    {V, element(2, X)}.

only_ws(Left) ->
    case erl_scan:tokens([], Left, 0) of
        {done, _, _} ->
            false;
        _ ->
            true
    end.

config_error() ->
    {error,
     {none, load_file,
      "configuration file must contain ONE list ended by <dot>"}}.

%% Merges envs for all apps.  Env2 overrides Env1
merge_env(Env1, Env2) ->
    merge_env(Env1, Env2, []).

merge_env([{App, AppEnv1} | T], Env2, Res) ->
    case get_env_key(App, Env2) of
	{value, AppEnv2, RestEnv2} ->
	    NewAppEnv = merge_app_env(AppEnv1, AppEnv2),
	    merge_env(T, RestEnv2, [{App, NewAppEnv} | Res]);
	_ ->
	    merge_env(T, Env2, [{App, AppEnv1} | Res])
    end;
merge_env([], Env2, Res) ->
    Env2 ++ Res.

%% Merges envs for an application.  Env2 overrides Env1
merge_app_env(Env1, Env2) ->
    merge_app_env(Env1, Env2, []).

merge_app_env([{Key, Val} | T], Env2, Res) ->
    case get_env_key(Key, Env2) of
	{value, NewVal, RestEnv} ->
	    merge_app_env(T, RestEnv, [{Key, NewVal}|Res]);
	_ ->
	    merge_app_env(T, Env2, [{Key, Val} | Res])
    end;
merge_app_env([], Env2, Res) ->
    Env2 ++ Res.

get_env_key(Key, Env) -> get_env_key(Env, Key, []).
get_env_key([{Key, Val} | T], Key, Res) ->
    {value, Val, T ++ Res};
get_env_key([H | T], Key, Res) ->
    get_env_key(T, Key, [H | Res]);
get_env_key([], _Key, Res) -> Res.
