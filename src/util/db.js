
var q = require('q');
var MongoClient = require('mongodb').MongoClient;
var ObjectID = require('mongodb').ObjectID;
var config = require('../config');

module.exports = {

  // Fail when it's failed to connect to the db
  isAvailable: function () {
    return this.exec((db) =>
              q.ninvoke(db, 'stats')
                .then((stats) => !!stats)
                .catch(() => false));
  },

  getId: (str) => new ObjectID(str),

  exec: (callback, scope) => {
    var dbRef;
    return q.nfcall(MongoClient.connect, config.get('dbConnectUrl'))
      .then((db) => {
        dbRef = db;
        return callback.call(scope, db);  // should return a promise
      })
      .finally(() => {
        dbRef.close();
      });
  }

};
