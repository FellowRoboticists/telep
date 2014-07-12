%%
%% This is a 'ranch' managed worker. When a socket client
%% connects to the server, the socket is passed here and
%% we manage the overall communication.
%%
%% When the client first connects, we challenge for a valid
%% message containing the robot's name. If we get a valid-
%% looking message, we put a 'registration|robot_name' message
%% into the 'registration' tube of beanstalk for processing,
%% then we drop into loop that watches the 'robot_name_commands'
%% tube and forward them onto the connected socket client.
%%
-module(robot_command_protocol).
-behavior(ranch_protocol).

-export([ start_link/4 ]).
-export([ init/4 ]).
-export([ send_message/3, send_messages/3 ]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
  { ok, Q } = beanstalk:connect(),
  { ok, Log } = syslog:open("robot_command_protocol", [ cons, perror, pid ], local3),
  { using, "registration" } = beanstalk:use(Q, registration),

	loop(init, Socket, Transport, Q, Log, nil),

  % If we get here, that means we have dropped out of the socket
  % loop. Close everthing out.

	ok = Transport:close(Socket),
  syslog:close(Log),
  ok = beanstalk:close(Q).

loop(init, Socket, Transport, Q, Log, _) ->
  % Challenge the socket client
  send_message(Socket, Transport, "And You Are?"),
  % Wait for response
  case receive_message(Socket, Transport) of
    { ok, Data } ->
      case string:tokens(Data, "|") of
        [ "robot", RobotName ] ->
          % Valid challenge response. Put a message in the queue
          { inserted, _ } = beanstalk:put(Q, binary:list_to_bin(io_lib:format("register|~s", [ RobotName ]))),
          loop(command_wait, Socket, Transport, Q, Log, RobotName);
        _ ->
          syslog:log(Log, notice, "Invalid challenge response: ~s", [ Data ])
      end;
    Error ->
      syslog:log(Log, notice, "Error receiving init message: ~p", [ Error ])
  end;

loop(command_wait, Socket, Transport, Q, Log, TubeName) ->
  { watching, _ } = beanstalk:watch(Q, io_lib:format("~s_commands", [ TubeName ])),
  % Wait on the queue for 10 seconds
  case beanstalk:reserve(Q, 10) of

    { reserved, JobId, BMessage } ->
      % Regardless of the disposition of the received message, we need to
      % delete the beanstalk job.
      { deleted } = beanstalk:delete(Q, JobId),
      case send_message(Socket, Transport, binary:bin_to_list(BMessage)) of
        ok ->
          % Message was sent OK, back around the loop we go.
          loop(command_wait, Socket, Transport, Q, Log, TubeName);
        Error ->
          kill_remaining_jobs(Q),
          { inserted, _ } = beanstalk:put(Q, binary:list_to_bin(io_lib:format("unregister|~s", [ TubeName ]))),
          syslog:log(Log, err, "Robot ~s closed the socket: ~p",  [ TubeName, Error ])
      end;

    { timed_out } ->
      % The idea here is to see if the socket is still alive
      % before we loop back into waiting on the queue. If we
      % don't do this, we won't know we lost the client until
      % we actually get a command from the queue to send.
      case send_message(Socket, Transport, "noop") of
        ok -> 
          % Socket is OK, back to waiting on the tube
          loop(command_wait, Socket, Transport, Q, Log, TubeName);
        Error ->
          % We don't have to worry about clearing out the queue here;
          % if we got a timeout, then by definition there are no jobs
          % on the queue.
          % OK, the socket is closed; notify of the de-registration
          % and drop out of the loop.
          { inserted, _ } = beanstalk:put(Q, binary:list_to_bin(io_lib:format("unregister|~s", [ TubeName ]))),
          syslog:log(Log, err, "Robot ~s closed the socket: ~p",  [ TubeName, Error ])
      end;

    Error ->
      % Log the error it and drop out of the loop.
      syslog:log(Log, err, "Error reserving a job: ~p",  [ Error ])
  end.

%% Private methods

kill_remaining_jobs(Q) ->
  case beanstalk:reserve(Q, 0) of
    { reserved, JobId, _BMessage } ->
      { deleted } = beanstalk:delete(Q, JobId),
      kill_remaining_jobs(Q); % Loop back until we get them all.

    { timed_out } ->
      ok
  end.


receive_message(Socket, Transport) ->
  case Transport:recv(Socket, 0, 10000) of
    { ok, BString } ->
      SignedMessage = binary:bin_to_list(BString),
      case gen_server:call(whereis(signature), { verify, SignedMessage }) of
        { ok, Message } -> { ok, Message };
        Error -> Error
      end;
    Error -> Error
  end.

send_messages(_, _, []) ->
  io:format("No commands to send~n");

send_messages(Socket, Transport, [Message|Messages]) ->
  send_message(Socket, Transport, Message),
  V = receive_message(Socket, Transport),
  io:format("Received message: ~p~n", [ V ]),
  send_messages(Socket, Transport, Messages).

send_message(Socket, Transport, Message) ->
  SignedMessage = gen_server:call(whereis(signature), { sign, Message }),
  Transport:send(Socket, SignedMessage).
