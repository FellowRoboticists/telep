#!/usr/bin/env node
var bs = require('nodestalker'),
    tube = 'test_tube',
    events = require('events');

function Commander() {
  events.EventEmitter.call(this);

  this.do = function(command) {
    this.emit('do', command);
  };
}

Commander.prototype.__proto__ = events.EventEmitter.prototype;

var commander = new Commander();

commander.on('do', function(command) {
  console.log("Command: " + command);
});

function processJob(job, callback) {
  // Doing something really expensive
  console.log('processing...' + job.data);
  commander.do(job.data);

  // setTimeout(function() { callback(); }, 1000);
  callback();
}

function reserveJob() {
  var client = bs.Client();
  client.watch(tube).onSuccess(function(data) {
    client.reserve().onSuccess(function(job) {
      process.nextTick(reserveJob); // Go back and listen for more
      processJob(job, function() {
        client.deleteJob(job.id).onSuccess(function(del_msg) {
          console.log('Deleted job: ' + job);
        });
      });
    });
  });
}

reserveJob();
