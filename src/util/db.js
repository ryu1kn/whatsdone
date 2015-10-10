
var q = require('q');
var MongoClient = require('mongodb').MongoClient;
var ObjectID = require('mongodb').ObjectID;

var dbRef;

module.exports = {

  /**
   * @param {string} dbConnectUrl
   * @return {q}
   */
  init: (dbConnectUrl) =>
    q.nfcall(MongoClient.connect, dbConnectUrl)
      .then((db) => {
        dbRef = db;
        return true;
      })
      .catch(() => false),

  /**
   * @param {string} str
   * @return {ObjectID}
   */
  getId: (str) => new ObjectID(str),

  /**
   * @param {Function} callback
   * @param {Object} scope
   * @return {*}
   */
  exec: (callback, scope) => callback.call(scope, dbRef)

};
