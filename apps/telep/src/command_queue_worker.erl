%%
%% This module creates a gen_server that is responsible for
%% enqueing the commands to a robot. It registers a named
%% PID - command_queuer - that other parts of the application
%% can use to submit requests to enqueue robot commands.
%%
-module(command_queue_worker).
-behavior(gen_server).

-export([ start_link/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

start_link() ->
  { ok, Pid } = gen_server:start_link(?MODULE, { }, []),
  % Register the process as a named command
  register(command_queuer, Pid),
  { ok, Pid }.

init({ }) ->
  { ok, Q } = beanstalk:connect(),
  { ok, { Q } }.

handle_call(_, _From, State) ->
  { noreply, State }.

handle_cast({ stop, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"stop">>),
  { noreply, { Q } };

handle_cast({ forward, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"forward">>),
  { noreply, { Q } };

handle_cast({ backward, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"backward">>),
  { noreply, { Q } };

handle_cast({ rotate_ccw, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"rotate_ccw">>),
  { noreply, { Q } };

handle_cast({ rotate_cw, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"rotate_cw">>),
  { noreply, { Q } };

handle_cast({ speed_up, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"speed_up">>),
  { noreply, { Q } };

handle_cast({ slow_down, Tube }, { Q }) ->
  { using, _ } = beanstalk:use(Q, Tube),
  { inserted, _ } = beanstalk:put(Q, <<"slow_down">>),
  { noreply, { Q } };

handle_cast(_, State) ->
  { noreply, State }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, { Q }) ->
  beanstalk:close(Q).

code_change(_OldVsn, State, _Extra) ->
  State.
