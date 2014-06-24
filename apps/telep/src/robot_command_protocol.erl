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
  % { watching, _ } = beanstalk:watch(Q, commands),
	loop(init, Socket, Transport, Q, Log, nil),
	ok = Transport:close(Socket),
  syslog:close(Log),
  ok = beanstalk:close(Q).

loop(init, Socket, Transport, Q, Log, _) ->
  send_message(Socket, Transport, "And You Are?"),
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
  case beanstalk:reserve(Q) of
    { reserved, JobId, BMessage } ->
      send_message(Socket, Transport, binary:bin_to_list(BMessage)),
      { deleted } = beanstalk:delete(Q, JobId),
      loop(command_wait, Socket, Transport, Q, Log, TubeName);
    Error ->
      syslog:log(Log, err, "Error reserving a job: ~p",  [ Error ])
  end.

%% Private methods

receive_message(Socket, Transport) ->
  case receive_message_length(Socket, Transport) of
    { ok, Length } ->
      receive_message_content(Socket, Transport, Length);
    Error ->
      Error
  end.

receive_message_length(Socket, Transport) ->
  case Transport:recv(Socket, 1, 10000) of
    { ok, BLength } ->
      { ok, binary:decode_unsigned(BLength) };
    Error ->
      Error
  end.

receive_message_content(Socket, Transport, Length) ->
  case Transport:recv(Socket, Length, 10000) of
    { ok, BString } ->
      { ok, binary:bin_to_list(BString) };
    Error ->
      Error
  end.

send_messages(_, _, []) ->
  io:format("No commands to send~n");

send_messages(Socket, Transport, [Message|Messages]) ->
  send_message(Socket, Transport, Message),
  V = receive_message(Socket, Transport),
  io:format("Received message: ~p~n", [ V ]),
  send_messages(Socket, Transport, Messages).

send_message(Socket, Transport, Message) ->
  Transport:send(Socket, binary:encode_unsigned(length(Message))),
  Transport:send(Socket, Message).
