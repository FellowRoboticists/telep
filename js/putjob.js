#!/usr/bin/env node

var bs = require('nodestalker'),
    tube = 'test_tube';


var client2 = bs.Client();
// Having set up the listener, just do a few messages to the tube
client2.use(tube).onSuccess(function(data) {
  client2.put('Job 1').onSuccess(function() {
      client2.disconnect();
  });
});
