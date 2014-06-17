var client = require('nodestalker').Client();

client.use('default').onSuccess(function(data) {
  console.log(data);

  client.put('my job').onSuccess(function(data) {
    console.log(data);
    client.disconnect();
  });
});
