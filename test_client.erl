%%
%% This module is used to send test registration messages to the
%% socket server and get commands.
%%
-module(test_client).
-export([ send_message/2 ]).

send_message(Port, Message) ->
  { ok, Sock } = gen_tcp:connect("localhost", Port, [ binary, { packet, 0 }, { active, false } ]),
  { ok, RecvMessage } = receive_message(Sock, gen_tcp),
  io:format("Got: ~s~n", [ RecvMessage ]),
  send_message(Sock, gen_tcp, Message),
  gen_tcp:close(Sock).


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
