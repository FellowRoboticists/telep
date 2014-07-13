// The following node packages should be installed:
//  * ws - for websocket support
//  * nodestalker - for beanstalk support
//

var cfg = { 
     ssl: true,
     port: 8081,
     ssl_key: '/etc/pki/tls/private/daneel-telep.key',
     ssl_cert: '/etc/pki/tls/certs/daneel-telep.crt'
};

var fs = require('fs');

var httpServ = (cfg.ssl) ? require('https') : require('http');

var WebSocketServer = require('ws').Server; 
    // wss = new WebSocketServer({port: 8081});

var bs = require('nodestalker'),
    tube = 'notify';

var app = null;

// dummy request processing
var processRequest = function( req, res ) {
  res.writeHead(200);
  res.end("All glory to WebSockets!\n");
}

//
// Define an object to keep track of whether the
// connection is still established
// 
function ConnectionState() {
  this.connected = true;
}

/**
 * Watches for notifications on the beanstalk 'notify'
 * tube and hands them off for processing.
 */
function watchForNotifications(ws, connectionState) {
  var client = bs.Client();
  client.watch(tube).onSuccess(function(data) {
    client.reserve().onSuccess(function(job) {
      console.log('received job' + job);
      if (connectionState.connected) {
        watchForNotifications(ws, connectionState);

        processNotification(ws, job, connectionState, function() {
          client.deleteJob(job.id).onSuccess(function(del_msg) {
            client.disconnect();
          });
        });
      }
    });
  });
}

if (cfg.ssl) {
  app = httpServ.createServer({
      // Providing server with ssl key/cert
      key: fs.readFileSync(cfg.ssl_key),
      cert: fs.readFileSync(cfg.ssl_cert)
      }, processRequest).listen(cfg.port);
} else {
  app = httpServ.createServer(processRequest).listen(cfg.port);
}

var wss = new WebSocketServer( { server: app } );

/**
 * An asynchronous function to process the notifications
 * received on the beanstalk 'notify' tube'.
 */
function processNotification(ws, job, connectionState, callback) {
  var components = job.data.split("|");
  var message = { type: components[0] === 'robot_registered' ? 'register' : 'unregister',
    data: { name: components[1] } };
  var json = JSON.stringify(message);
  ws.send(json, function(err) {
    if (err) {  
      console.log(err);
      connectionState.connected = false;
    }
  });

  setTimeout(function() { callback(); }, 1000)
}

// Deal with connections to the websocket server
wss.on('connection', function(ws) {

  // A connection was established; do something
  var connectionState = new ConnectionState();

  watchForNotifications(ws, connectionState);

  ws.on('close', function() {
    console.log("The connection was closed");
    connectionState.connected = false;
  });

});
