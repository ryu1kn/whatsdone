
const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');
const WrappedError = require('../WrappedError');

class DynamoTableClient {

  constructor({collectionName, idName}) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._uuidGenerator = ServiceLocator.uuidGenerator;
    this._collectionName = collectionName;
    this._idName = idName;
  }

  getById(id) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id)
    };
    return this._docClient.get(params).promise()
      .then(response => response.Item)
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  getByIds(ids) {
    if (_.isEmpty(ids)) return Promise.resolve([]);
    const uniqIds = _.uniq(ids).filter(id => id);
    const params = {
      RequestItems: {
        [this._getTableName()]: {
          Keys: uniqIds.map(id => this._toIdObject(id))
        }
      }
    };
    return this._docClient.batchGet(params).promise()
      .then(response => response.Responses[this._getTableName()])
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  // @deprecated
  getByQuery(query) {
    const params = Object.assign(
      this._composeScanQuery(query),
      {TableName: this._getTableName()}
    );
    return this._docClient.scan(params).promise()
      .then(result => _.get(result, 'Items[0]'))
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  put(newData) {
    const id = this._uuidGenerator.generate();
    const params = {
      TableName: this._getTableName(),
      Item: Object.assign({}, newData, this._toIdObject(id))
    };
    return this._docClient.put(params).promise()
      .then(() => id)
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  delete(id) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id)
    };
    return this._docClient.delete(params).promise()
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  update(id, newData) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id),
      AttributeUpdates: this._getAttributeUpdatesValues(newData)
    };
    return this._docClient.update(params).promise()
      .then(() => this.getById(id))   // XXX: Don't query again
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  _toIdObject(id) {
    return {[this._idName]: id};
  }

  _getTableName() {
    return this._collectionName;
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

module.exports = DynamoTableClient;
