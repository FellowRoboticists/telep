%% 
%% This module defines a worker responsible for registration of a robot in
%% the database and forwarding the message along for notification.
%%
-module(registration_worker).
-export([ start_link/0 ]).

start_link() ->
  Pid = spawn_link(queue_listener, 
                   connect_and_listen, 
                   [ registration, fun store_and_notify_registration/4 ]),
  { ok, Pid }.

store_and_notify_registration(Message, Q, Conn, Log) ->
  case string:tokens(Message, "|") of
    [ "register", Robot ] ->
      register_robot(Q, Conn, Log, Robot);
    [ "unregister", Robot ] ->
      unregister_robot(Q, Conn, Log, Robot);
    _ ->
      syslog:log(Log, err, "Unknown message received: ~p", [ Message ])
  end.

register_robot(Q, Conn, Log, Robot) ->
  syslog:log(Log, info, "Registering robot (~p) in the database", [ Robot ]),
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
          syslog:log(Log, debug, "Fixing to update database record"),
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, true, updated_at, queue_listener:time_stamp() }} ),
          syslog:log(Log, debug, "Done updating database record")
      end
    end
  ),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_registered|~s", [ Robot ]))).

unregister_robot(Q, Conn, Log, Robot) ->
  mongo:do(safe, master, Conn, registrations, fun() ->
      case mongo:find_one(registrations, { robot, Robot, registered, true }) of
        {} ->
          syslog:log(Log, err, "Unable to find db entry for robot unregistration: ~p", [ Robot ]);
        { Doc } ->
          % Found the record, now we need to update the updated at and registration fields.
          % Parse out the values of the record
          { '_id', Id, robot, _Robot, registered, _Registered, created_at, _CreatedAt, updated_at, _UpdatedAt } = Doc,
          syslog:log(Log, debug, "Fixing to update database record"),
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, false, updated_at, queue_listener:time_stamp() }} ),
          syslog:log(Log, debug, "Done updating database record")
      end
    end
  ),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_unregistered|~s", [ Robot ]))).
