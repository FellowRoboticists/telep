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

handle_cast(_, State) ->
  { noreply, State }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, _S=#persist{dbconn=Conn}) ->
  % Should be polite and close the database connection.
  mongo:disconnect(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

