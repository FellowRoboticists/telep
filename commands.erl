-module(commands).
-export([ stop/3, forward/3, backward/3, rotate_ccw/3, rotate_cw/3, speed_up/3, slow_down/3 ]).
-export([path1/3]).

path1(SessionID, _Env, _Input) ->
  io:format("Environment: ~p~n", [ _Env ]),
  % { _, QueryString } = lists:keyfind(query_string, 1, _Env),
  % { content_length, ContentLength } = lists:keyfind(content_length, 1, _Env),
  mod_esi:deliver(SessionID, "Hello from path1").
  % mod_esi:deliver(SessionID, "Query string: "),
  % mod_esi:deliver(SessionID, QueryString),
  % mod_esi:deliver(SessionID, "Content length: "),
  % mod_esi:deliver(SessionID, ContentLength).
  %

stop(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|stop").

forward(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|forward").

backward(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|backward").

rotate_ccw(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|rotate_ccw").

rotate_cw(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|rotate_cw").

speed_up(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|speed_up").

slow_down(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, "command|slow_down").
