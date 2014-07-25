%%
%% This module provides methods to parse out 
%% URL parameters. The goal is to return the
%% the params to a list of name/value pairs.
%%
-module(q_params).
-export([ parse_query_parameters/1 ]).

-type nvp() :: nonempty_string().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec parse_name_value(nvp()) -> tuple().

parse_name_value(Nvp) ->
  case string:tokens(Nvp, "=") of
    [ Name, Value ] ->
      { string:strip(Name), string:strip(Value) };
    [ Name ] ->
      Index = string:str(string:strip(Nvp), "="),
      if Index > 1 -> { string:strip(Name), "" }
       ; Index =< 1 -> {}
      end;
    _ ->
      {}
  end.

-spec convert_to_params(list(string()), list(tuple())) -> list(tuple()).

convert_to_params([], NewList) ->
  NewList;
convert_to_params([Head|Tail], NewList) ->
  case parse_name_value(Head) of
    { Name, Value } ->
      convert_to_params(Tail, [ { Name, Value } | NewList ]);
    {} ->
      convert_to_params(Tail, NewList)
  end.

-spec parse_query_parameters(string()) -> list(tuple()).

parse_query_parameters(QString) ->
  Nvps = string:tokens(QString, "&"),
  convert_to_params(Nvps, []).

-ifdef(TEST).
parse_name_value_test_() ->
  [
    ?_assertMatch({ "key", "value" }, parse_name_value("key=value")),
    ?_assertMatch({ "key", "value" }, parse_name_value(" key = value ")),
    ?_assertMatch({}, parse_name_value("=")),
    ?_assertMatch({"key", "" }, parse_name_value("key=")),
    ?_assertMatch({}, parse_name_value("=value")),
    ?_assertMatch({}, parse_name_value("key|value")),
    ?_assertMatch({}, parse_name_value("key=value=other"))
  ].

convert_to_params_test_() ->
  [
    ?_assertMatch([ { "key", "value" }], convert_to_params([ "key=value" ], [])),
    ?_assertMatch([ { "key", "value" }, { "key1", "value1" } ], convert_to_params([ "key1=value1", "key=value" ], [])),
    ?_assertMatch([], convert_to_params([], [])),
    ?_assertMatch([], convert_to_params([ "" ], [])),
    ?_assertMatch([ { "k2", "" }, { "k1", "v1" } ], convert_to_params([ "k1=v1", "", "k2=" ], []))
  ].

parse_query_parameters_test_() ->
  [
    ?_assertMatch([ { "k1", "v1" } ], parse_query_parameters("k1=v1")),
    ?_assertMatch([ { "k2", "v2" }, { "k1", "v1" } ], parse_query_parameters("k1=v1&k2=v2")),
    ?_assertMatch([ { "k2", "v2" }, { "k1", "v1" } ], parse_query_parameters("k1=v1&k2=v2&")),
    ?_assertMatch([ { "k3", ""   }, { "k2", "v2" }, { "k1", "v1" } ], parse_query_parameters("k1=v1&k2=v2&k3=")),
    ?_assertMatch([ ], parse_query_parameters("&")),
    ?_assertMatch([ ], parse_query_parameters("&&"))
  ].
-endif.

