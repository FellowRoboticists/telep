var net = require('net');
var sprintf = require('sprintf').sprintf;

var args = process.argv.slice(2);

var client = new net.Socket();
var first = true;
// var name = "robot|Sammy";
var name = args[0] || "Sammy";
var registrationStr = sprintf("robot|%s", name);

client.connect(5555, 'daneel', function() {
    console.log('Connected');
});

client.on('data', function(data) {
    console.log('Data: ' + data);
    if (first) {
      client.write(sprintf("%c%s", registrationStr.length, registrationStr));
      first = false;
    }
    // client.destroy();
});

client.on('close', function(data) {
    console.log('Connection closed');
});
