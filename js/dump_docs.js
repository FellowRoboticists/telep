var MongoClient = require('mongodb').MongoClient,
    format = require('util').format;

MongoClient.connect('mongodb://localhost:27017/registrations', function(err, db) {
    if (err) throw err;

    var collection = db.collection('registrations');

    collection.count(function(err, count) {
      console.log(format("count = %s", count));
      });

    collection.find().toArray(function(err, results) {
      console.dir(results);
      // Close the db
      db.close();
      });
    });
