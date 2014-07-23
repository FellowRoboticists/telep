%%
%% This module provides methods to parse out 
%% URL parameters. The goal is to return the
%% the params to a list of name/value pairs.
%%
-module(q_params).
-export([ parse_query_parameters/1 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse_name_value(Nvp) ->
  [ Name, Value ] = string:tokens(Nvp, "="),
  { string:strip(Name), string:strip(Value) }.

convert_to_params([], NewList) ->
  NewList;
convert_to_params([Head|Tail], NewList) ->
  convert_to_params(Tail, [parse_name_value(Head)|NewList]).

parse_query_parameters(QString) ->
  Nvps = string:tokens(QString, "&"),
  convert_to_params(Nvps, []).

-ifdef(TEST).
parse_name_value_test_() ->
  [?_assertMatch({ "key", "value" }, parse_name_value("key=value")),
   ?_assertMatch({ "key", "value" }, parse_name_value(" key = value ")),
   ?_assertError({badmatch,["key|value"]}, parse_name_value("key|value")),
   ?_assertError({badmatch,["key","value","other"]}, parse_name_value("key=value=other"))
  ].
-endif.

