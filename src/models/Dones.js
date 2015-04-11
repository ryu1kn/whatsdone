
var q = require('q');

var db = require('../util/db');

function getAllDones (db) {
  return q.ninvoke(db.collection('dones').find({}, {_id: 0}), 'toArray')
    .catch(() => '[]');
}

function putDone (db, newData) {
  return q.ninvoke(db.collection('dones'), 'insert', newData);
}

module.exports = {

  /**
   * @return {Q}
   */
  read: () => db.exec((db) => getAllDones(db)),

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write: (newData) =>
      db.exec((db) =>
        putDone(db, newData)
        .then((result) => {
          if (result.result.ok === 1) {
            return getAllDones(db);
          } else {
            throw new Error('Failed to save the given data');
          }
        }))

};
