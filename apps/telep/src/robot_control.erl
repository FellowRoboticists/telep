%
% This is a 'view' module for mustache. It is leveraged by the
% robot_control.mustache template.
%
-module(robot_control).
-export([ registered_robots/ 0 ]).

registered_robots() ->
  Robots = gen_server:call(whereis(robot_db), { registered_robots }),
  [ dict:from_list([{ robot, RobotName }]) || RobotName <- Robots ].
