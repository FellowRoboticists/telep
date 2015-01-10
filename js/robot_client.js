#!/usr/bin/env node
// -*- javascript -*-

var net = require('net');
var sprintf = require('sprintf').sprintf;
var fs = require('fs');
var dsigner = require('dsigner');

var keyPath = process.env.KEY_PATH;
if (! keyPath) {
  console.error("No KEY_PATH environment variable specified");
  process.exit(1);
}

var args = process.argv.slice(2);

var client = new net.Socket();
var first = true;
var name = args[0] || "minion";
var registrationStr = sprintf("robot|%s", name);

function verifyServerMessage(keyPath, name, data) {
  var cmps = data.toString().split('|');
  var message = cmps[0];
  var signature = cmps[1];
  if (dsigner.verifySignatureFor(keyPath, name, message, signature)) {
    return message;
  } else {
    return null;
  }
}

function signMessage(keyPath, name, message) {
  var signature = dsigner.signMessageFor(keyPath, name, message);
  return message + "|" + signature;
}

client.connect(5555, 'giskard', function() {
    console.log('Connected');
});

client.on('data', function(data) {
    var message = verifyServerMessage(keyPath, 'telep', data);
    if (message) {
      console.log('Data: ' + message);
      if (first) {
        client.write(signMessage(keyPath, name, registrationStr));
        first = false;
      }
    } else {
      console.log("Invalid message from server: " + data);
    }
});

client.on('close', function(data) {
    console.log('Connection closed');
});
