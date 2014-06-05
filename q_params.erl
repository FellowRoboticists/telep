-module(q_params).
-export([ parse_query_parameters/1 ]).

parse_name_value(Nvp) ->
  [ Name, Value ] = string:tokens(Nvp, "="),
  { Name, Value }.

convert_to_params([], NewList) ->
  NewList;
convert_to_params([Head|Tail], NewList) ->
  convert_to_params(Tail, [parse_name_value(Head)|NewList]).

parse_query_parameters(QString) ->
  Nvps = string:tokens(QString, "&"),
  convert_to_params(Nvps, []).
