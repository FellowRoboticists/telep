var MongoClient = require('mongodb').MongoClient,
    format = require('util').format;

function bin2string(array) {
  var result = "";
  for (var i=0; i<array.length; i++) {
    result += String.fromCharCode(array[i]);
  }
  return result;
}

function bin2datestring(array) {
  var result = "";

  result += bin2string(array[0]); // Year
  result += String.fromCharCode(array[1]); // Dash
  result += bin2string(array[2]); // Month
  result += String.fromCharCode(array[3]); // Dash
  result += bin2string(array[4]); // Day
  result += String.fromCharCode(array[5]); // Space
  result += bin2string(array[6]); // Hour
  result += String.fromCharCode(array[7]); // Colon
  result += bin2string(array[8]); // Minute
  result += String.fromCharCode(array[9]); // Colon
  result += bin2string(array[10]); // Second

  return result;
}

MongoClient.connect('mongodb://localhost:27017/registrations', function(err, db) {
    if (err) throw err;

    var collection = db.collection('registrations');

    collection.count(function(err, count) {
      console.log(format("count = %s", count));
      });

    collection.find().toArray(function(err, results) {
      console.dir(results);
      var len = results.length
      for (var i=0; i<len; i++) {
        console.log(format("Robot: %s", bin2string(results[i].robot)));
        console.log(format("Registered: %s", results[i].registered));
        // console.dir(results[i].created_at);
        console.log(format("Created at: %s", bin2datestring(results[i].created_at)));
        console.log(format("Updated at: %s", bin2datestring(results[i].updated_at)));
        console.log("---------------------------------------------------");
      }
      // Close the db
      db.close();
      });
    });
