'use strict';

let _ = require('lodash');
let q = require('q');
let dbUtil = require('../util/db');

const ID_FIELD = 'id';

class Database {

  constructor(collectionName) {
    this._collectionName = collectionName;
    console.info('Collection `%s` is ready', this._collectionName);
  }

  getAll() {
    return dbUtil.exec(db =>
      q.ninvoke(db.collection(this._collectionName).find(), 'toArray')
        .then(items => items.map(item => this._swapIdField(item)))
        .catch(() => '[]'));   // XXX: Why string?
  }

  getById(id) {
    return dbUtil.exec(db =>
      this._getItemByQuery(db, {_id: dbUtil.getId(id)}, this._collectionName)
        .then(item => item ? this._swapIdField(item) : null));
  }

  getByIds(ids) {
    return dbUtil.exec(db =>
      q.ninvoke(db.collection(this._collectionName).find({
        _id: {
          $in: _.compact(_.uniq(ids))
                .map((id) => dbUtil.getId(id))
        }
      }), 'toArray')
      .catch(() => []));
  }

  getByQuery(query) {
    return dbUtil.exec(db => this._getItemByQuery(db, query, this._collectionName));
  }

  put(newData) {
    return dbUtil.exec(db =>
      q.ninvoke(db.collection(this._collectionName), 'insert', newData)
        .then(result => {
          if (result.result.ok === 1) {
            return result.insertedIds[0];
          } else {
            throw new Error('Failed to save the given data');
          }
        }));
  }

  delete(id) {
    return dbUtil.exec(db =>
      q.ninvoke(db.collection(this._collectionName), 'deleteOne', {
        _id: dbUtil.getId(id)
      }));
  }

  update(id, newData) {
    return dbUtil.exec(db =>
      q.ninvoke(db.collection(this._collectionName), 'findOneAndUpdate', {
        _id: dbUtil.getId(id)
      }, {
        $set: newData
      })
      .then(result => result.value));
  }

  _swapIdField(item) {
    if (!item._id) {
      return item;
    }
    item[ID_FIELD] = item._id;
    delete item._id;
    return item;
  }

  _getItemByQuery(db, query, collection) {
    return q.ninvoke(db.collection(collection), 'findOne', query)
      .catch(() => null);
  }

}

module.exports = Database;
