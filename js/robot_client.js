var net = require('net');
var sprintf = require('sprintf').sprintf;

var client = new net.Socket();
var first = true;
var name = "robot|Sammy";

client.connect(5555, 'localhost', function() {
    console.log('Connected');
});

client.on('data', function(data) {
    console.log('Data: ' + data);
    if (first) {
      client.write(sprintf("%c%s", name.length, name));
      first = false;
    }
    // client.destroy();
});

client.on('close', function(data) {
    console.log('Connection closed');
});
