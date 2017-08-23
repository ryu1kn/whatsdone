
const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');
const WrappedError = require('../WrappedError');

class DynamoTableClient {

  constructor(collectionName) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._uuidGenerator = ServiceLocator.uuidGenerator;
    this._collectionName = collectionName;
  }

  getById(id) {
    const params = {
      TableName: this._getTableName(),
      Key: {id}
    };
    return this._docClient.get(params).promise()
      .then(response => response.Item)
      .catch(this._wrapError);
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
    return this._docClient.batchGet(params).promise()
      .then(response => response.Responses[this._getTableName()])
      .catch(this._wrapError);
  }

  // @deprecated
  getByQuery(query) {
    const params = Object.assign(
      this._composeScanQuery(query),
      {TableName: this._getTableName()}
    );
    return this._docClient.scan(params).promise()
      .then(result => _.get(result, 'Items[0]'))
      .catch(this._wrapError);
  }

  put(newData) {
    const id = this._uuidGenerator.generate();
    const params = {
      TableName: this._getTableName(),
      Item: Object.assign({}, newData, {id})
    };
    return this._docClient.put(params).promise()
      .then(() => id)
      .catch(this._wrapError);
  }

  delete(id) {
    const params = {
      TableName: this._getTableName(),
      Key: {id}
    };
    return this._docClient.delete(params).promise()
      .catch(this._wrapError);
  }

  update(id, newData) {
    const params = {
      TableName: this._getTableName(),
      Key: {id},
      AttributeUpdates: this._getAttributeUpdatesValues(newData)
    };
    return this._docClient.update(params).promise()
      .then(() => this.getById(id))   // XXX: Don't query again
      .catch(this._wrapError);
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

  _wrapError(e) {
    throw new WrappedError(e);
  }

}

module.exports = DynamoTableClient;
