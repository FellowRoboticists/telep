// You need to install the ws package into npm:
// npm install ws
//
var WebSocketServer = require('ws').Server, wss = new WebSocketServer({port: 8080});

wss.on('connection', function(ws) {
  console.log('connection established');
    ws.on('message', function(message) {
      console.log('received: %s', message);
    });
    ws.send('"something"');
});
