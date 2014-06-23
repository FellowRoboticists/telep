-module(web_server).
-export([ start/0 ]).

start() ->
  { ok, WebConfigPath } = application:get_env(config_path),
  inets:start(),
  inets:start(httpd, [{ proplist_file, WebConfigPath }]).
