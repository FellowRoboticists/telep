// The following node packages should be installed:
//  * ws - for websocket support
//  * nodestalker - for beanstalk support
//
var WebSocketServer = require('ws').Server, 
    wss = new WebSocketServer({port: 8081});

var bs = require('nodestalker'),
    tube = 'notify';

/**
 * An asynchronous function to process the notifications
 * received on the beanstalk 'notify' tube'.
 */
function processNotification(ws, job, callback) {
  ws.send("Received notification: " + job.data);

  setTimeout(function() { callback(); }, 1000)
}

/**
 * Watches for notifications on the beanstalk 'notify'
 * tube and hands them off for processing.
 */
function watchForNotifications(ws) {
  var client = bs.Client();
  client.watch(tube).onSuccess(function(data) {
    client.reserve().onSuccess(function(job) {
      console.log('received job' + job);
      watchForNotifications(ws);

      processNotification(ws, job, function() {
        client.deleteJob(job.id).onSuccess(function(del_msg) {
          client.disconnect();
        });
      });
    });
  });
}

// Deal with connections to the websocket server
wss.on('connection', function(ws) {

  // A connection was established; do something

  console.log('connection established');

  watchForNotifications(ws);

});
