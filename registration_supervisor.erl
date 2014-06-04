%% 
%% This module defines a supervisor process to manage the workers
%% that are responsible for registration and notification.
%%
-module(registration_supervisor).
-behavior(supervisor).

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
  { ok, { { one_for_one, 3, 60 },
      [ { registrar, 
          { registration_worker, start_link, [] },
          permanent, 1000, worker, [ registration_worker ]},
        { notifier,
          { notification_worker, start_link, [] },
          permanent, 1000, worker, [ notification_worker ]},
        { command_queuer,
          { command_queue_worker, start_link, [] },
          permanent, 1000, worker, [ command_queue_worker ]},
        { commander_worker,
          { command_worker, start_link, [ 5555 ] },
          permanent, 1000, worker, [ command_worker ]}
        ]}}.
