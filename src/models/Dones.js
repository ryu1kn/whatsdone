
var q = require('q');

var dbUtil = require('../util/db');

function getDoneById(db, id) {
  return q.ninvoke(db.collection('dones'), 'findOne', {_id: dbUtil.getId(id)})
    .then((done) => {
      done.id = done._id;
      delete done._id;
      return done;
    });
}

function getAllDones(db) {
  return q.ninvoke(db.collection('dones').find(), 'toArray')
    .then((dones) => dones.map((done) => {
      done.id = done._id;
      delete done._id;
      return done;
    }))
    .catch(() => '[]');
}

function putDone(db, newData) {
  return q.ninvoke(db.collection('dones'), 'insert', newData);
}

function deleteDone(db, id) {
  return db.collection('dones').deleteOne({_id: dbUtil.getId(id)});
}


module.exports = {

  /**
   * @return {Q}
   */
  read: () => dbUtil.exec((db) => getAllDones(db)),

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write: (newData) =>
      dbUtil.exec((db) =>
        putDone(db, newData)
        .then((result) => {
          if (result.result.ok === 1) {
            return getDoneById(db, result.insertedIds[0]);
          } else {
            throw new Error('Failed to save the given data');
          }
        })),

  /**
   * @param {string} id
   */
  remove: (id) => dbUtil.exec((db) => deleteDone(db, id))

};
