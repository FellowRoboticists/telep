var net = require('net');
var sprintf = require('sprintf').sprintf;
var fs = require('fs');
var crypto = require('crypto');

var args = process.argv.slice(2);

var client = new net.Socket();
var first = true;
var name = args[0] || "minion";
var registrationStr = sprintf("robot|%s", name);

// Pull in the private key for the specific robot
var privateKeyFile = "./" + name + "_private.pem";
var privateKeyData = null;
fs.readFile(privateKeyFile, 'ascii', function(err, data) {
    if (err) {
      return console.log(err);
    }
    privateKeyData = data;
});

// Pull in the public key for the telep server
var publicKeyFile = "./telep_public.pem";
var publicKeyData = null;
fs.readFile(publicKeyFile, 'ascii', function(err, data) {
    if (err) {
      return console.log(err);
    }
    publicKeyData = data;
});

function verifyServerMessage(data) {
  var cmps = data.toString().split('|');
  var message = cmps[0];
  var signature = cmps[1];
  var verifier = crypto.createVerify('RSA-SHA256');
  verifier.update(message);
  if (verifier.verify(publicKeyData, signature, 'base64')) {
    return message;
  } else {
    return null;
  }
}

function signMessage(message) {
  var signer = crypto.createSign('RSA-SHA256');
  signer.update(message);
  var signature = signer.sign(privateKeyData, 'base64');
  return message + "|" + signature;
}

client.connect(5555, 'daneel', function() {
    console.log('Connected');
});

client.on('data', function(data) {
    var message = verifyServerMessage(data);
    if (message) {
      console.log('Data: ' + message);
      if (first) {
        client.write(signMessage(registrationStr));
        // client.write(sprintf("%c%s", registrationStr.length, registrationStr));
        first = false;
      }
    } else {
      console.log("Invalid message from server: " + data);
    }
    // client.destroy();
});

client.on('close', function(data) {
    console.log('Connection closed');
});
