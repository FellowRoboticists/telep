%% 
%% This module defines the pages supported by the telep application
%%
-module(pages).
-export([ robot_control/3 ]).

robot_control(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID, gen_server:call(whereis(templater), { view, robot_control }) ).

