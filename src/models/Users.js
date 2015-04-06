
var q = require('q');
var sha1 = require('sha1');
var MongoClient = require('mongodb').MongoClient;

var config = require('../config');

/**
 * @param {} db
 * @param {email, password} user
 */
function findUser(db, user) {
  return q.ninvoke(db.collection('users'), 'findOne', {
      email: user.email,
      password: sha1(user.password)
    })
    .catch(() => null);
}

module.exports = {

  findUser: (loginInfo) =>
    q.nfcall(MongoClient.connect, config.get('dbConnectUrl'))
      .then((db) => {
        return findUser(db, loginInfo)
          .finally(() => {
            db.close();
          });
      })

};
