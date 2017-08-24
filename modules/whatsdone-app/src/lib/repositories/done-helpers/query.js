
const _ = require('lodash');
const ServiceLocator = require('../../ServiceLocator');
const WrappedError = require('../../WrappedError');
const utils = require('../utils');

const DEFAULT_SCAN_LIMIT = 20;

class DoneQueryHelper {

  constructor(collectionName) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._dateProvider = ServiceLocator.dateProvider;
    this._logger = ServiceLocator.logger;
    this._collectionName = collectionName;
  }

  query(nextKey) {
    const restoredKey = this._decodeNextKey(nextKey);
    const params = Object.assign(
      {
        TableName: this._collectionName,
        IndexName: 'date',
        Limit: DEFAULT_SCAN_LIMIT,
        KeyConditionExpression: '#month = :m',
        ExpressionAttributeNames: {
          '#month': 'month',
          '#date': 'date'
        },
        ExpressionAttributeValues: this._getExpressionAttributeValues(restoredKey),
        ScanIndexForward: false,
        ProjectionExpression: 'id, #date, doneThing, userId',
        Select: 'SPECIFIC_ATTRIBUTES'
      },
      restoredKey && {ExclusiveStartKey: restoredKey}
    );
    this._logger.log('query params:', JSON.stringify(params));
    return this._docClient.query(params).promise()
      .then(response => this._buildResponse(response))
      .catch(e => {
        throw new WrappedError(e, params);
      });
  }

  _getExpressionAttributeValues(restoredKey) {
    if (restoredKey) {
      return {
        ':m': restoredKey.month
      };
    }
    const currentDate = this._dateProvider.getCurrentDate().toISOString();
    return {
      ':m': currentDate.substr(0, utils.MONTH_LENGTH)
    };
  }

  _decodeNextKey(nextKey) {
    return nextKey ? utils.getDoneWithMonth(JSON.parse(nextKey)) : null;
  }

  _encodeNextKey(keyObject) {
    return keyObject && JSON.stringify(_.omit(keyObject, 'month'));
  }

  _buildResponse(queryResult) {
    return {
      items: queryResult.Items.map(done => _.omit(done, 'month')),
      nextKey: this._encodeNextKey(queryResult.LastEvaluatedKey)
    };
  }

}

module.exports = DoneQueryHelper;
