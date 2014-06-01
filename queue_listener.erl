-module(queue_listener).
-export([ connect_and_listen/2, echo_message/3, store_and_notify_registration/3 ]).
-export([ start_registration_link/0, start_notify_link/0 ]).
-export([ time_stamp/0 ]).

% Here's the setup we need
% application:start(bson).
% application:start(mongodb).
% { ok, Conn } = mongo:connect(localhost).

start_registration_link() ->
  spawn_link(?MODULE, connect_and_listen, [ boob, fun store_and_notify_registration/3 ]).

start_notify_link() ->
  spawn_link(?MODULE, connect_and_listen, [ notify, fun echo_message/3 ]).

connect_and_listen(Tube, ReceiveFun) ->
  case beanstalk:connect() of
    { ok, Q } ->
      io:format("Successful connection to beanstalkd~n"),
      % Now, attempt to connect to the database
      case mongo:connect(localhost) of
        { ok, Conn } ->
          io:format("Successful connection to database~n"),
          register_and_listen_to_queue(Q, Tube, ReceiveFun, Conn),
          % Make sure we shut stuff down when we should
          beanstalk:close(Q),
          mongo:disconnect(Conn);
        _Me ->
          io:format("Error connecting to db: ~p~n", [ _Me ])
      end;
    _E ->
      io:format("Error connecting to beanstalk: ~p~n", [ _E ])
  end.

register_and_listen_to_queue(Q, Tube, ReceiveFun, Conn) ->
  beanstalk:watch(Q, Tube),
  beanstalk:use(Q, notify),
  process_queue(Q, ReceiveFun, Conn).

process_queue(Q, ReceiveFun, Conn) ->
  case beanstalk:reserve(Q) of
    { reserved, JobId, Message } -> 
      try 
        ReceiveFun(binary:bin_to_list(Message), Q, Conn) 
      of
        _ -> ok
      catch 
        _ -> error
      after
        io:format("Deleting job: ~p~n", [ JobId ]),
        beanstalk:delete(Q, JobId)
      end
  end,
  process_queue(Q, ReceiveFun, Conn).

echo_message(Message, _Q, _Conn) ->
  io:format("Received Message: ~p~n", [ Message ]).

store_and_notify_registration(Message, Q, Conn) ->
  case string:tokens(Message, "|") of
    [ "register", Robot ] ->
      register_robot(Q, Conn, Robot);
    [ "unregister", Robot ] ->
      unregister_robot(Q, Conn, Robot);
    _ ->
      io:format("Unknown message received: ~p~n", [ Message ])
  end.

time_stamp() ->
  iso_8601_fmt(calendar:local_time()).

%% Private functions

iso_8601_fmt(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  % io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
  io_lib:format("~p-~p-~p ~p:~p:~p",
                [Year, Month, Day, Hour, Min, Sec]).


register_robot(Q, Conn, Robot) ->
  io:format("Registering robot (~p) in the database~n", [ Robot ]),
  mongo:do(safe, master, Conn, registrations, fun() ->
      case mongo:find_one(registrations, { robot, Robot }) of
        {} -> 
          % Didn't find the robot
          % Inserting the robot
          mongo:insert(registrations, { robot, Robot,
                                      registered, true,
                                      created_at, time_stamp(),
                                      updated_at, time_stamp() });
        { Doc } ->
          % Found the record, now we need to update the updated at and registration fields.
          % Parse out the values of the record
          { '_id', Id, robot, _Robot, registered, _Registered, created_at, _CreatedAt, updated_at, _UpdatedAt } = Doc,
          io:format("Fixing to update database record~n"),
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, true, updated_at, time_stamp() }} ),
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
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, false, updated_at, time_stamp() }} ),
          io:format("Done updating database record~n")
      end
    end
  ),
  beanstalk:put(Q, binary:list_to_bin(io_lib:format("robot_unregistered|~s", [ Robot ]))).
