
// TODO: This class knows to much about MongoDB. Separate it.

var q = require('q');

var dbUtil = require('../util/db');
var Done = require('./Done');

function getDoneById(db, id) {
  return q.ninvoke(db.collection('dones'), 'findOne', {_id: dbUtil.getId(id)})
    .then(done => {
      // TODO: Rewrite with Done class
      done.id = done._id;
      delete done._id;
      return done;
    });
}

function getAllDones(db) {
  return q.ninvoke(db.collection('dones').find(), 'toArray')
    .then(dones => dones.map(done => {
      // TODO: Rewrite with Done class
      done.id = done._id;
      delete done._id;
      return done;
    }))
    .catch(() => '[]');
}

function putDone(db, newData) {
  return q.ninvoke(db.collection('dones'), 'insert', newData);
}

function deleteDone(db, id, currentUserId) {
  return q.ninvoke(db.collection('dones'), 'findOne', {_id: dbUtil.getId(id)})
    .then(found => {
      if (found === null) {
        throw new Error('[NotFound]: Done item not found');
      }
      if (found.userId !== currentUserId) {
        throw new Error('[AccessDeined]: You don\'t have the permission to delete this item.');
      }
      return q.ninvoke(db.collection('dones'), 'deleteOne', {_id: dbUtil.getId(id)});
    });
}

function updateDone(db, id, currentUserId, newData) {
  return q.ninvoke(db.collection('dones'), 'findOne', {_id: dbUtil.getId(id)})
    .then(found => {
      if (found === null) {
        throw new Error('[NotFound]: Done item not found');
      }
      if (found.userId !== currentUserId) {
        throw new Error('[AccessDeined]: You don\'t have the permission to modify this item.');
      }
      return q.ninvoke(db.collection('dones'), 'findOneAndUpdate', {
          _id: dbUtil.getId(id)
        }, {
          $set: Done.getModifiable(newData)
        }).then(result => {
          var done = result.value;
          return new Done(done._id, done.userId, done.doneThing, done.date);
        });
    });
}

module.exports = {

  /**
   * @return {Q}
   */
  read: () => dbUtil.exec(db => getAllDones(db)),

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write: (newData) =>
      dbUtil.exec(db =>
        putDone(db, newData)
        .then(result => {
          if (result.result.ok === 1) {
            return getDoneById(db, result.insertedIds[0]);
          } else {
            throw new Error('Failed to save the given data');
          }
        })),

  /**
   * @param {string} id
   * @param {string} currentUserId
   */
  remove: (id, currentUserId) =>
      dbUtil.exec(db => deleteDone(db, id, currentUserId)),

  /**
   * @param {string} id
   * @param {string} currentUserId
   * @param {{doneThing: string, date: string}} newData
   */
  update: (id, currentUserId, newData) =>
      dbUtil.exec(db => updateDone(db, id, currentUserId, newData))

};
