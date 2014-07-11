%%
%% This module defines the active web actions for the web server.
%% Think of each method in here as a controller action. Kind of
%% a CGI thing.
%%
-module(commands).
-export([ stop/3, forward/3, backward/3, rotate_ccw/3, rotate_cw/3, speed_up/3, slow_down/3 ]).

stop(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, stop).

forward(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, forward).

backward(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, backward).

rotate_ccw(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, rotate_ccw).

rotate_cw(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, rotate_cw).

speed_up(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, speed_up).

slow_down(SessionID, Env, _Input) ->
  accept_web_request(SessionID, Env, slow_down).

%% Private methods

accept_web_request(SessionID, Env, Command) ->
  RobotName = parse_robot_name(Env),
  case gen_server:call(whereis(robot_db), { robot_registered, RobotName }) of
    true ->
      % The robot is registered, go ahead and send the command
      gen_server:cast(whereis(command_queuer), { Command, format_tube_name(RobotName) }),
      mod_esi:deliver(SessionID, io_lib:format("command|~s", [ Command ]));
    _ ->
      mod_esi:deliver(SessionID, io_lib:format("Robot ~s is not registered", [ RobotName ]) )
  end.

format_tube_name(RobotName) ->
  io_lib:format("~s_commands", [ RobotName ]).

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
