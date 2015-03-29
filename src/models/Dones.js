
var q = require('q');
var MongoClient = require('mongodb').MongoClient;

function getAllDones (db) {
  return q.ninvoke(db.collection('dones').find({}, {_id: 0}), 'toArray')
    .catch(() => '[]');
}

function putDone (db, newData) {
  return q.ninvoke(db.collection('dones'), 'insert', newData);
}

module.exports = {

  DbConnectUrl: 'mongodb://localhost:27017/whatsdone',

  /**
   * @return {Q}
   */
  read: () =>
    q.nfcall(MongoClient.connect, this.DbConnectUrl)
      .then((db) => {
        return getAllDones(db)
          .catch(() => '[]')
          .finally(() => {
            db.close();
          });
      }),

  /**
   * @param {{doneThing: string, date: string}}
   */
  write: (newData) =>
    q.nfcall(MongoClient.connect, this.DbConnectUrl)
      .then((db) => {
        return putDone(db, newData)
          .then((result) => {
            if (result.result.ok === 1) {
              return getAllDones(db);
            } else {
              throw new Error('Failed to save the given data');
            }
          })
          .finally(() => {
            db.close();
          });
      })

};
