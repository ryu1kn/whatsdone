
var q = require('q');
var sha1 = require('sha1');

var dbUtil = require('../util/db');

/**
 * @param {} db
 * @param {Object} query
 */
function findUser(db, query) {
  return q.ninvoke(db.collection('users'), 'findOne', query)
    .catch(() => null);
}

module.exports = {

  findUser: (loginInfo) =>
      dbUtil.exec((db) => findUser(db, {
        email   : loginInfo.email,
        password: sha1(loginInfo.password)
      })),

  /**
   * @param {string} id
   * @return {q}
   */
  getById: (id) =>
    dbUtil.exec((db) =>
      findUser(db, {
        _id: dbUtil.getId(id)
      }))

};
