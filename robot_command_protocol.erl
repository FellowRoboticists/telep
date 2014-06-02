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
  { using, "registration" } = beanstalk:use(Q, registration),
  { watching, _ } = beanstalk:watch(Q, commands),
	loop(init, Socket, Transport, Q),
	ok = Transport:close(Socket),
  ok = beanstalk:close(Q).

loop(init, Socket, Transport, Q) ->
  send_message(Socket, Transport, "And You Are?"),
  case receive_message(Socket, Transport) of
    { ok, Data } ->
      case string:tokens(Data, "|") of
        [ "robot", _RobotName ] ->
          % Valid challenge response. Put a message in the queue
          { inserted, _ } = beanstalk:put(Q, binary:list_to_bin(io_lib:format("register|~s", [ _RobotName ]))),
          loop(command_wait, Socket, Transport, Q);
        _ ->
          io:format("Invalid challenge response: ~s~n", [ Data ])
      end;
    Error ->
      io:format("Error receiving init message: ~p~n", [ Error ])
  end;

loop(command_wait, Socket, Transport, Q) ->
  case beanstalk:reserve(Q) of
    { reserved, _JobId, BMessage } ->
      send_message(Socket, Transport, binary:bin_to_list(BMessage)),
      loop(command_wait, Socket, Transport, Q);
    Error ->
      io:format("Error reserving a job: ~p~n",  [ Error ])
  end.

%% Private methods

receive_message(Socket, Transport) ->
  { ok, Length } = receive_message_length(Socket, Transport),
  receive_message_content(Socket, Transport, Length).

receive_message_length(Socket, Transport) ->
  { ok, BLength } = Transport:recv(Socket, 1, 10000),
  { ok, binary:decode_unsigned(BLength) }.

receive_message_content(Socket, Transport, Length) ->
  { ok, BString } = Transport:recv(Socket, Length, 10000),
  { ok, binary:bin_to_list(BString) }.

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
