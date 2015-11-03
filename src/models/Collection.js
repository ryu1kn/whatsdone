
var _ = require('lodash');
var q = require('q');
var dbUtil = require('../util/db');

var ID_FIELD = 'id';

function swapIdField(item) {
  if (!item._id) {
    return item;
  }
  item[ID_FIELD] = item._id;
  delete item._id;
  return item;
}

function getAll(db, collection) {
  return q.ninvoke(db.collection(collection).find(), 'toArray')
    .then(items => items.map(item => swapIdField(item)))
    .catch(() => '[]');
}

function getItemById(db, id, collection) {
  return getItemByQuery(db, {_id: dbUtil.getId(id)}, collection)
    .then(item => item ? swapIdField(item) : null);
}

function getItemsByIds(db, ids, collection) {
  return q.ninvoke(db.collection(collection).find({
      _id: {
        $in: _.compact(_.uniq(ids))
              .map((id) => dbUtil.getId(id))
      }
    }), 'toArray')
    .catch(() => []);
}

function getItemByQuery(db, query, collection) {
  return q.ninvoke(db.collection(collection), 'findOne', query)
    .catch(() => null);
}

/**
 * @param {db}
 * @param {collection}
 * @param {Object}
 * @return {Q}
 */
function putItem(db, collection, newData) {
  return q.ninvoke(db.collection(collection), 'insert', newData)
    .then(result => {
      if (result.result.ok === 1) {
        return result.insertedIds[0];
      } else {
        throw new Error('Failed to save the given data');
      }
    });
}

function deleteItem(db, collection, id) {
  return q.ninvoke(db.collection(collection), 'deleteOne', {_id: dbUtil.getId(id)});
}

function updateItem(db, collection, id, newData) {
  return q.ninvoke(db.collection(collection), 'findOneAndUpdate', {
      _id: dbUtil.getId(id)
    }, {
      $set: newData
    })
    .then(result => result.value);
}

/**
 * @constructor
 * @param {string} collectionName
 */
function Database(collectionName) {
  this._collectionName = collectionName;
}

Database.prototype = {

  getAll: function () {
    return dbUtil.exec(db => getAll(db, this._collectionName));
  },

  getById: function (id) {
    return dbUtil.exec(db => getItemById(db, id, this._collectionName));
  },

  getByIds: function (ids) {
    return dbUtil.exec(db => getItemsByIds(db, ids, this._collectionName));
  },

  getByQuery: function (query) {
    return dbUtil.exec(db => getItemByQuery(db, query, this._collectionName));
  },

  put: function (newData) {
    return dbUtil.exec(db => putItem(db, this._collectionName, newData));
  },

  delete: function (id) {
    return dbUtil.exec(db => deleteItem(db, this._collectionName, id));
  },

  update: function (id, newData) {
    return dbUtil.exec(db => updateItem(db, this._collectionName, id, newData));
  }

};
Database.prototype.constructor = Database;

module.exports = Database;
