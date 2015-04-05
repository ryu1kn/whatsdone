
var q = require('q');
var sha1 = require('sha1');
var MongoClient = require('mongodb').MongoClient;

var dbConnectUrl = process.env.DB_URI_KEY &&
                   process.env[process.env.DB_URI_KEY];

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

  DbConnectUrl: dbConnectUrl || 'mongodb://localhost:27017/whatsdone',

  authenticate: (loginInfo) =>
    q.nfcall(MongoClient.connect, this.DbConnectUrl)
      .then((db) => {
        return authenticate(db, loginInfo)
          .finally(() => {
            db.close();
          });
      })

};
