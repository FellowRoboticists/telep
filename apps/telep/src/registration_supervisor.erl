%% 
%% This module defines a supervisor process to manage the workers
%% that are responsible for registration and notification.
%%
-module(registration_supervisor).
-behavior(supervisor).

-export([ start/2, stop/1, start_link/0 ]).
-export([ init/1 ]).

start(normal, _Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

stop(_State) ->
  ok.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
  { ok, { { one_for_one, 3, 60 },
      [ { registrar, 
          { registration_worker, start_link, [] },
          permanent, 1000, worker, [ registration_worker ]},
% No longer need this worker to be supervised. We are using
% a node application to provide the beanstalk listener/
% communications.
%        { notifier,
%          { notification_worker, start_link, [] },
%          permanent, 1000, worker, [ notification_worker ]},
        { command_queuer,
          { command_queue_worker, start_link, [] },
          permanent, 1000, worker, [ command_queue_worker ]},
        { commander_worker,
          { command_worker, start_link, [ 5555 ] },
          permanent, 1000, worker, [ command_worker ]},
        { web_server,
          { web_server, start, [] },
          permanent, 1000, worker, [ web_server ]},
        { signature_handler,
          { signature_handler, start_link, [ "/etc/telep", "telep" ] },
          permanent, 1000, worker, [ signature_handler ] },
        { template_processor,
          { template_processor, start_link, [ ] },
          permanent, 1000, worker, [ template_processor ] },
        { robot_db,
          { robot_db, start_link, [ ] },
          permanent, 1000, worker, [ robot_db ] }
        ]}}.
