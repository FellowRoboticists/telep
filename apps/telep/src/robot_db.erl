% 
% This module provides methods to deal with robot queries
%
-module(robot_db).
-behavior(gen_server).

-record(persist, { dbconn } ).

-export([ start_link/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

start_link() ->
  { ok, Pid } = gen_server:start_link(?MODULE, [ ], []),
  register(robot_db, Pid),
  { ok, Pid }.

init([]) ->
  % We want to start out with a mongodb connection
  % in our state.
  { ok, Conn } = mongo:connect(localhost),
  { ok, #persist{dbconn=Conn} }.

handle_call({ registered_robots }, _From, S=#persist{dbconn=Conn}) ->
  {ok, RobotList } = mongo:do(safe, master, Conn, registrations, fun() ->
        Cursor = mongo:find(registrations, { registered, true }),
        RobotList = build_robot_list(Cursor),
        mongo:close_cursor(Cursor),
        RobotList
    end),
  { reply, RobotList, S };

handle_call({ robot_registered, RobotName }, _From, S=#persist{dbconn=Conn}) ->
  { ok, Registered } = mongo:do(safe, master, Conn, registrations, fun() ->
        case mongo:find_one(registrations, { robot, RobotName, registered, true }) of
          {} ->
            % Didn't find the robot or it wasn't registered
            false;
          { _Doc } ->
            true
        end
    end
  ),
  { reply, Registered, S }.

handle_cast({ register_robot, RobotName }, S=#persist{dbconn=Conn}) ->
  handle_robot_registration(RobotName, true, Conn),
  { noreply, S };

handle_cast({ unregister_robot, RobotName }, S=#persist{dbconn=Conn}) ->
  handle_robot_registration(RobotName, false, Conn),
  { noreply, S }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, _S=#persist{dbconn=Conn}) ->
  % Should be polite and close the database connection.
  mongo:disconnect(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

% Private Methods

build_robot_list(Cursor) ->
  case mongo:next(Cursor) of
    {} -> []; % Nothing in the cursor
    { Doc } -> 
      { RobotName } = bson:lookup(robot, Doc),
      build_robot_list(Cursor, [ RobotName ])
  end.

build_robot_list(Cursor, RobotList) ->
  case mongo:next(Cursor) of
    {} -> RobotList; % Nothing more in the cursor
    { Doc } -> 
      { RobotName } = bson:lookup(robot, Doc),
      build_robot_list(Cursor, [ RobotName | RobotList ])
  end.

handle_robot_registration(RobotName, Registered, Conn) ->
  mongo:do(safe, master, Conn, registrations, fun() ->
      case mongo:find_one(registrations, { robot, RobotName }) of
        {} -> 
          % Didn't find the robot, inserting a new one
          mongo:insert(registrations, { robot, RobotName,
                                      registered, Registered,
                                      created_at, queue_listener:time_stamp(),
                                      updated_at, queue_listener:time_stamp() });
        { Doc } ->
          % Found the record, now we need to update the updated at and registration fields.
          % Parse out the values of the record
          { '_id', Id, robot, _Robot, registered, _Registered, created_at, _CreatedAt, updated_at, _UpdatedAt } = Doc,
          mongo:modify(registrations, { '_id', Id }, { '$set', { registered, Registered, updated_at, queue_listener:time_stamp() }} )
      end
    end).
