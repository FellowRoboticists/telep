# This makefile is used to do a number of things for us:
#
# 1. Deploy the artifacts of the application
# 2. Run some of the more common rebar commands
#

test_it: out/thing.it

out/thing.it: out butter
	@echo "The target: $@"
	@echo "Dependent on: $^"

out:
	@echo "mkdir -p $@"

butter:
	@echo "Just butter"

# 
# The main target for deploying the ws_server
#
deploy_ws_server: /etc/init.d/ws_server /etc/sysconfig/ws_server /usr/local/ws_server/ws_server.js /var/log/ws_server

/etc/init.d/ws_server: scripts/ws_server
	cp $< $@

/etc/sysconfig/ws_server: config/ws_server
	cp $< $@

/usr/local/ws_server/ws_server.js: js/ws_server.js /usr/local/ws_server 
	cp $< $@

/usr/local/ws_server:
	mkdir -p $@

/var/log/ws_server:
	mkdir -p $@
	chgrp telep $@
	chmod g+w $@
