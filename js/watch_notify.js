var bs = require('nodestalker'),
    tube = 'test_tube';

function processJob(job, callback) {
  // Doing something really expensive
  console.log('processing...' + job.data);

  setTimeout(function() { callback(); }, 1000);
}

function resJob() {
  var client = bs.Client();
  client.watch(tube).onSuccess(function(data) {
    client.reserve().onSuccess(function(job) {
      console.log('received job', job);
      resJob();

      processJob(job, function() {
        client.deleteJob(job.id).onSuccess(function(del_msg) {
          console.log('deleted', job);
          console.log(del_msg);
          client.disconnect();
        });
        console.log('processed', job);
      });
    });
  });
}

resJob();
