-module(web_server).
-export([ start/0 ]).

start() ->
  inets:start(),
  inets:start(httpd, [{ proplist_file, "8080.conf" }]).
