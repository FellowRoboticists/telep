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
	sudo cp $< $@

/etc/sysconfig/ws_server: config/ws_server
	sudo cp $< $@

/usr/local/ws_server/ws_server.js: js/ws_server.js /usr/local/ws_server 
	sudo cp $< $@

/usr/local/ws_server:
	sudo mkdir -p $@

/var/log/ws_server:
	sudo -- sh -c "mkdir -p $@; chgrp telep $@; chmod g+w $@"

/var/log/telep:
	sudo -- sh -c "mkdir -p $@; chgrp telep $@; chmod g+w $@"

#
# The main target for deploying the telep application
#
deploy_telep: /etc/telep/telep.conf /etc/telep/user.lst /etc/telep/group.lst /var/log/telep deploy_assets
	./rebar compile
	./rebar generate
	sudo cp -R rel/telep /usr/local
	sudo chmod 755 /usr/local/telep/bin/telep

/etc/telep/telep.conf: 8080.conf /etc/telep
	sudo cp $< $@

/etc/telep/user.lst: user.lst /etc/telep
	sudo cp $< $@

/etc/telep/group.lst: group.lst /etc/telep
	sudo cp $< $@

/etc/telep:
	sudo mkdir -p $@

ts := $(shell /bin/date +"%Y%m%d%H%M%S")
release_dir = /var/www/telep/releases/$(ts)

deploy_assets: /var/www/telep/releases
	mkdir -p $(release_dir)
	cp -R www/* $(release_dir)
	rm -f /var/www/telep/current
	ln -s $(release_dir) /var/www/telep/current

/var/www/telep/releases: /var/www/telep
	mkdir -p $@

/var/www/telep:
	sudo mkdir -p $@
	sudo chgrp deployers $@
	sudo chmod 2775 $@
