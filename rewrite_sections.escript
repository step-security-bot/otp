#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A 1 +SDio 1 +S 1 -mode minimal
%% This script can be used to convert <seealso> to <see*>
%% Invoked like this:
%%  find lib/*/doc/src system/doc/*/ erts/doc/src/ '(' -name *.xml -o -name *.xmlsrc ')' -exec ./rewrite-sections.escript {} \;
%% You need a OTP with built documentation for this to work

main([File]) ->
    io:format("Parse: ~s~n",[File]),
    {ok, B} = file:read_file(File),

    _CurrApp =
        case string:lexemes(File,"/") of
            ["erts"|_] ->
                "erts";
            ["lib",App|_] ->
                {lib,App};
            ["system","doc",App|_] ->
                {system,App}
        end,

    case get_link_type(B) of
        "seeerl" ->
            NewB = replace_section(B),
            case iolist_to_binary(NewB) of
                B -> ok;
                _Else ->
                    file:write_file(File, NewB),
                    %%io:format("~s",[NewB]),
                    ok
            end;
        _ ->
            ok
    end.

get_link_type(B) when is_binary(B) ->
  case {re:run(B, <<"<erlref>">>),
        re:run(B, <<"<appref>">>),
        re:run(B, <<"<comref>">>),
        re:run(B, <<"<cref>">>),
        re:run(B, <<"<fileref>">>),
        re:run(B, <<"<chapter>">>)} of
      {{match,_},_,_,_,_,_} ->
          "seeerl";
      {_,{match,_},_,_,_,_} ->
          "seeapp";
      {_,_,{match,_},_,_,_} ->
          "seecom";
      {_,_,_,{match,_},_,_} ->
          "seecref";
      {_,_,_,_,{match,_},_} ->
          "seefile";
      {_,_,_,_,_,{match,_}} ->
          "seeguide";
      _ ->
          {match, _} = re:run(B, <<"<application|<part|<specs|<internal">>),
          ignore
  end;
get_link_type(File) ->
    {ok,B} = file:read_file(File),
    get_link_type(B).


replace_section(B) ->
    B1 = re:replace(B,"<section>([\\s]*<title>See Also.*)</section>",
                    "<seealso>\\1</seealso>",
                    [dotall,caseless]),
    re:replace(B1,"<section>(\\s*<title>Callback.*)</section>\\s*<funcs>(.*)</funcs>",
               "<callbacks>\n    <description>\\1  </description>\\2</callbacks>",
               [dotall,caseless]).

%% part(_Bin,{-1,0}) ->
%%     <<>>;
%% part(Bin, Part) ->
%%     binary:part(Bin,Part).
%% part(Bin,Pos,Len) ->
%%     part(Bin,{Pos,Len}).
