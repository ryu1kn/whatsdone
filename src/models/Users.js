
var q = require('q');
var sha1 = require('sha1');
var MongoClient = require('mongodb').MongoClient;

var config = require('../config');

/**
 * @param {} db
 * @param {email, password} user
 */
function authenticate (db, user) {
  return q.ninvoke(db.collection('users').find({
      email: user.email,
      password: sha1(user.password)
    }), 'toArray')
    .then((found) => found.length !== 0)
    .catch(() => '[]');
}

module.exports = {

  authenticate: (loginInfo) =>
    q.nfcall(MongoClient.connect, config.get('dbConnectUrl'))
      .then((db) => {
        return authenticate(db, loginInfo)
          .finally(() => {
            db.close();
          });
      })

};
