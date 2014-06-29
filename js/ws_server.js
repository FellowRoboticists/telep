// The following node packages should be installed:
//  * ws - for websocket support
//  * nodestalker - for beanstalk support
//

var cfg = { 
     ssl: true,
     port: 8081,
     ssl_key: '/home/dsieh/Projects/telep/server.key',
     ssl_cert: '/home/dsieh/Projects/telep/server.crt'
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

/**
 * An asynchronous function to process the notifications
 * received on the beanstalk 'notify' tube'.
 */
function processNotification(ws, job, callback) {
  ws.send(job.data);

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

// Deal with connections to the websocket server
wss.on('connection', function(ws) {

  // A connection was established; do something

  watchForNotifications(ws);

});
