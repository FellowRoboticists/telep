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

store_and_notify_registration(Message, Q, Conn) ->
  case string:tokens(Message, "|") of
    [ "register", Robot ] ->
      register_robot(Q, Conn, Robot);
    [ "unregister", Robot ] ->
      unregister_robot(Q, Conn, Robot);
    _ ->
      io:format("Unknown message received: ~p~n", [ Message ])
  end.

register_robot(Q, Conn, Robot) ->
  io:format("Registering robot (~p) in the database~n", [ Robot ]),
  mongo:do(safe, master, Conn, registrations, fun() ->
      case mongo:find_one(registrations, { robot, Robot }) of
        {} -> 
          % Didn't find the robot
          % Inserting the robot
          mongo:insert(registrations, { robot, Robot,
                                      registered, true,
                                      created_at, queue_listener:time_stamp(),
                                      updated_at, queue_listener:time_stamp() });
        { Doc } ->
          % Found the record, now we need to update the updated at and registration fields.
          % Parse out the values of the record
          { '_id', Id, robot, _Robot, registered, _Registered, created_at, _CreatedAt, updated_at, _UpdatedAt } = Doc,
          io:format("Fixing to update database record~n"),
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, true, updated_at, queue_listener:time_stamp() }} ),
          io:format("Done updating database record~n")
      end
    end
  ),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_registered|~s", [ Robot ]))).

unregister_robot(Q, Conn, Robot) ->
  mongo:do(safe, master, Conn, registrations, fun() ->
      case mongo:find_one(registrations, { robot, Robot, registered, true }) of
        {} ->
          io:format("Unable to find db entry for robot unregistration: ~p~n", [ Robot ]);
        { Doc } ->
          % Found the record, now we need to update the updated at and registration fields.
          % Parse out the values of the record
          { '_id', Id, robot, _Robot, registered, _Registered, created_at, _CreatedAt, updated_at, _UpdatedAt } = Doc,
          io:format("Fixing to update database record~n"),
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, false, updated_at, queue_listener:time_stamp() }} ),
          io:format("Done updating database record~n")
      end
    end
  ),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_unregistered|~s", [ Robot ]))).
