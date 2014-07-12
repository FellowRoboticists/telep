%% 
%% This module defines a worker responsible for registration of a robot in
%% the database and forwarding the message along for notification.
%%
-module(registration_worker).
-export([ start_link/0 ]).

start_link() ->
  Pid = spawn_link(queue_listener, 
                   connect_and_listen, 
                   [ registration, fun store_and_notify_registration/3 ]),
  { ok, Pid }.

store_and_notify_registration(Message, Q, Log) ->
  case string:tokens(Message, "|") of
    [ "register", Robot ] ->
      register_robot(Q, Log, Robot);
    [ "unregister", Robot ] ->
      unregister_robot(Q, Log, Robot);
    _ ->
      syslog:log(Log, err, "Unknown message received: ~p", [ Message ])
  end.

register_robot(Q, Log, Robot) ->
  syslog:log(Log, info, "Registering robot (~p) in the database", [ Robot ]),
  gen_server:cast(whereis(robot_db), { register_robot, Robot }),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_registered|~s", [ Robot ]))).

unregister_robot(Q, _Log, Robot) ->
  gen_server:cast(whereis(robot_db), { unregister_robot, Robot }),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_unregistered|~s", [ Robot ]))).
