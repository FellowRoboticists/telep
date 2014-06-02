%%
%% This module is used by a supervisor to start up a ranch accept pool
%% for connections to the robot.
%%
-module(command_worker).
-export([ start_link/1 ]).

start_link(Port) ->
  ranch:start_listener(commander, 
                       100, 
                       ranch_tcp, 
                       [{port, Port}], 
                       robot_command_protocol, []).
