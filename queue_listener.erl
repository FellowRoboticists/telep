%%
%% This module defines a somewhat generic pattern to connect to
%% beanstalk and listen for beanstalk messages.
%%
-module(queue_listener).
-export([ connect_and_listen/2 ]).
-export([ time_stamp/0 ]).

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

time_stamp() ->
  iso_8601_fmt(calendar:local_time()).

%% Private functions

iso_8601_fmt(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  % io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
  io_lib:format("~p-~p-~p ~p:~p:~p",
                [Year, Month, Day, Hour, Min, Sec]).


