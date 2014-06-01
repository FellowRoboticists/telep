%% 
%% A worker module to listen for messages coming into beanstalk
%% on the notification tube.
%%
-module(notification_worker).
-export([ start_link/0 ]).

start_link() ->
  Pid = spawn_link(queue_listener, 
                   connect_and_listen, 
                   [ notify, fun echo_message/3 ]),
  { ok, Pid }.

%%
%% Simply echo the message read from the notify queue to stdout
%%
echo_message(Message, _Q, _Conn) ->
  io:format("Received Message: ~p~n", [ Message ]).

