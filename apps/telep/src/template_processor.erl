%
% This module wraps the functionality of the mustache template
% engine.
%
-module(template_processor).
-behavior(gen_server).

-record(env, { server_root, document_root }).

-export([ start_link/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

start_link() ->
  {ok, Pid } = gen_server:start_link(?MODULE, [], []),
  register(templater, Pid),
  { ok, Pid }.

init([]) ->
  % Get the environment information from the web server
  [{server_root,ServerRoot},{document_root,DocumentRoot}] = httpd:info(whereis(httpd_instance_sup_22443), [server_root,document_root]),
  % Tell mustache where the templates are
  application:set_env(mustache, templates_dir, filename:join(DocumentRoot, "templates")),
  { ok, #env{server_root=ServerRoot, document_root=DocumentRoot } }.

handle_call({ text, TemplateText, Context }, _From, S) ->
  Processed = mustache:render(TemplateText, dict:from_list(Context)),
  { reply, Processed, S };

handle_call({ view, Module }, _From, S) ->
  Processed = mustache:render(Module),
  { reply, Processed, S }.

handle_cast(_, State) ->
  { noreply, State }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

