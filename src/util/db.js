'use strict';

let q = require('q');
let MongoClient = require('mongodb').MongoClient;
let ObjectID = require('mongodb').ObjectID;

class DbUtil {

  /**
   * @param {string} dbConnectUrl
   * @return {q}
   */
  init(dbConnectUrl) {
    return q.nfcall(MongoClient.connect, dbConnectUrl)
      .then((db) => {
        this._dbRef = db;
        return true;
      })
      .catch(() => false);
  }

  /**
   * @param {string} str
   * @return {ObjectID}
   */
  getId(str) {
    return new ObjectID(str);
  }

  /**
   * @param {Function} callback
   * @param {Object} scope
   * @return {*}
   */
  exec(callback, scope) {
    return callback.call(scope, this._dbRef);
  }

}

module.exports = new DbUtil();