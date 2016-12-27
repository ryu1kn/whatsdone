'use strict';

let _ = require('lodash');
let context = require('../service/context');
let q = require('q');
var Uuid = require('uuid');

class Database {

  constructor(collectionName) {
    var docClient = context.getDynamoDBDocumentClient();
    this._getItem = q.nbind(docClient.get, docClient);
    this._batchGetItems = q.nbind(docClient.batchGet, docClient);
    this._scanItems = q.nbind(docClient.scan, docClient);
    this._putItem = q.nbind(docClient.put, docClient);
    this._deleteItem = q.nbind(docClient.delete, docClient);
    this._updateItems = q.nbind(docClient.update, docClient);
    this._collectionName = collectionName;
    console.info('Collection `%s` is ready', this._collectionName);
  }

  getAll() {
    return this._scanItems({
      TableName: this._getTableName()
    })
    .then(response => response.Items);
  }

  getById(id) {
    return this._getItem({
      TableName: this._getTableName(),
      Key: {id}
    })
    .then(response => response.Item);
  }

  getByIds(ids) {
    const uniqIds = _.uniq(ids).filter(id => id);
    const params = {
      RequestItems: {
        [this._getTableName()]: {
          Keys: uniqIds.map(id => ({id}))
        }
      }
    };
    return this._batchGetItems(params)
      .then(response => response.Responses[this._getTableName()]);
  }

  // @deprecated
  getByQuery(query) {
    return this._scanItems(Object.assign(this._composeScanQuery(query), {
      TableName: this._getTableName()
    }))
    .then(result => _.get(result, 'Items[0]'));
  }

  put(newData) {
    let id = this._generateId();
    return this._putItem({
      TableName: this._getTableName(),
      Item: Object.assign({}, newData, {id})
    })
    .then(() => id);
  }

  delete(id) {
    return this._deleteItem({
      TableName: this._getTableName(),
      Key: {id}
    });
  }

  update(id, newData) {
    return this._updateItems({
      TableName: this._getTableName(),
      Key: {id},
      AttributeUpdates: this._getAttributeUpdatesValues(newData)
    })
    .then(() => this.getById(id));
  }

  _generateId() {
    return Uuid.v4();
  }

  _getTableName() {
    return 'whatsdone-' + this._collectionName;
  }

  _getAttributeUpdatesValues(newData) {
    return Object.keys(newData).reduce((result, key) => {
      result[key] = {
        Action: 'PUT',
        Value: newData[key]
      };
      return result;
    }, {});
  }

  _composeScanQuery(matchCondition) {
    let filterExpressions = [];
    let expressionAttributeValues = {};

    Object.keys(matchCondition).forEach(key => {
      filterExpressions.push(`${key} = :${key}`);
      expressionAttributeValues[`:${key}`] = matchCondition[key];
    });
    return {
      FilterExpression: filterExpressions.join(' AND '),
      ExpressionAttributeValues: expressionAttributeValues
    };
  }

}

module.exports = Database;
