#!/usr/bin/env node
var bs = require('nodestalker'),
    tube = 'test_tube',
    events = require('events');

var client = bs.Client();

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

function reserveJob() {
  client.watch(tube).onSuccess(function(data) {
    client.reserve().onSuccess(function(job) {
      process.nextTick(reserveJob); // Go back and listen for more
      commander.do(job.data);
      client.deleteJob(job.id).onSuccess(function(del_msg) {
        console.log('Deleted job: ' + job);
      });
    });
  });
}

process.nextTick(reserveJob);

console.log("Waiting for job to come in");
