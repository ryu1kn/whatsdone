
var q = require('q');
var sha1 = require('sha1');

var db = require('../util/db');

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
      db.exec((db) => findUser(db, loginInfo))

};
