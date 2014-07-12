%%
%% This module defines a somewhat generic pattern to connect to
%% beanstalk and listen for beanstalk messages.
%%
-module(queue_listener).
-export([ connect_and_listen/2 ]).
-export([ time_stamp/0 ]).

connect_and_listen(Tube, ReceiveFun) ->
  { ok, Log } = syslog:open("queue_listener", [ cons, perror, pid ], local3),
  case beanstalk:connect() of
    { ok, Q } ->
      register_and_listen_to_queue(Q, Tube, Log, ReceiveFun),
      % Make sure we shut stuff down when we should
      beanstalk:close(Q),
      syslog:close(Log);
    _E ->
      syslog:log(Log, err, "Error connecting to beanstalk: ~p", [ _E ])
  end.

register_and_listen_to_queue(Q, Tube, Log, ReceiveFun) ->
  beanstalk:watch(Q, Tube),
  beanstalk:use(Q, notify),
  process_queue(Q, Log, ReceiveFun).

process_queue(Q, Log, ReceiveFun) ->
  case beanstalk:reserve(Q) of
    { reserved, JobId, Message } -> 
      try 
        ReceiveFun(binary:bin_to_list(Message), Q, Log) 
      of
        _ -> ok
      catch 
        _ -> error
      after
        syslog:log(Log, info, "Deleting job: ~p", [ JobId ]),
        beanstalk:delete(Q, JobId)
      end
  end,
  process_queue(Q, Log, ReceiveFun).

time_stamp() ->
  iso_8601_fmt(calendar:local_time()).

%% Private functions

iso_8601_fmt(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  io_lib:format("~p-~p-~p ~p:~p:~p",
                [Year, Month, Day, Hour, Min, Sec]).


