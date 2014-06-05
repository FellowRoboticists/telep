-module(commands).
-export([ stop/3, forward/3, backward/3, rotate_ccw/3, rotate_cw/3, speed_up/3, slow_down/3 ]).
-export([path1/3]).

path1(SessionID, Env, _Input) ->
  io:format("Environment: ~p~n", [ Env ]),
  % { _, QueryString } = lists:keyfind(query_string, 1, Env),
  % { content_length, ContentLength } = lists:keyfind(content_length, 1, Env),
  mod_esi:deliver(SessionID, "Hello from path1").
  % mod_esi:deliver(SessionID, "Query string: "),
  % mod_esi:deliver(SessionID, QueryString),
  % mod_esi:deliver(SessionID, "Content length: "),
  % mod_esi:deliver(SessionID, ContentLength).
  %

stop(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { stop, RobotName }),
  mod_esi:deliver(SessionID, "command|stop").

forward(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { forward, RobotName }),
  mod_esi:deliver(SessionID, "command|forward").

backward(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { backward, RobotName }),
  mod_esi:deliver(SessionID, "command|backward").

rotate_ccw(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { rotate_ccw, RobotName }),
  mod_esi:deliver(SessionID, "command|rotate_ccw").

rotate_cw(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { rotate_cw, RobotName }),
  mod_esi:deliver(SessionID, "command|rotate_cw").

speed_up(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { speed_up, RobotName }),
  mod_esi:deliver(SessionID, "command|speed_up").

slow_down(SessionID, Env, _Input) ->
  RobotName = parse_robot_name(Env),
  gen_server:cast(whereis(command_queuer), { slow_down, RobotName }),
  mod_esi:deliver(SessionID, "command|slow_down").


%% Private methods

parse_robot_name(Env) ->
  case lists:keyfind(query_string, 1, Env) of
    { query_string, QString } ->
      Params = q_params:parse_query_parameters(QString);
    _ ->
      Params = []
  end,
  case lists:keyfind("robot", 1, Params) of
    { "robot", Name } ->
      Name;
    _ ->
      "anonymous"
  end.
