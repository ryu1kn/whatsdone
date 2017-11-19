
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
    return this._query(this._decodeNextKey(nextKey))
      .catch(e => {
        throw new WrappedError(e);
      });
  }

  _query(startKey) {
    const queryUntil = (params, accumulatedResponse) => {
      this._logger.log('query params:', JSON.stringify(params));
      return this._docClient.query(params).promise()
        .then(queryResult => {
          const next = {
            Items: [...accumulatedResponse.Items, ...queryResult.Items],
            LastEvaluatedKey: queryResult.LastEvaluatedKey
          };
          if (next.Items.length >= DEFAULT_SCAN_LIMIT) return next;
          const nextParams = this._buildQueryParamsFromMonthKey(this._getPrevMonthKey(params));
          return queryUntil(nextParams, next);
        });
    };
    const params = this._buildQueryFromStartKey(startKey);
    return queryUntil(params, {Items: []}).then(response => this._buildResponse(response));
  }

  _getPrevMonthKey(params) {
    function pad0(number) {
      return number < 10 ? `0${number}` : number;
    }
    const d = new Date(params.ExpressionAttributeValues[':m']);
    const oldMonth = d.getMonth() === 0 ? 12 : d.getMonth();
    const oldYear = d.getMonth() === 0 ? d.getFullYear() - 1 : d.getFullYear();
    return `${oldYear}-${pad0(oldMonth)}`;
  }

  _buildQueryParamsFromMonthKey(monthKey) {
    return this._buildQueryParams({monthKey});
  }

  _buildQueryFromStartKey(startKey) {
    return this._buildQueryParams({
      monthKey: this._getMonthKey(startKey),
      exclusiveStartKey: startKey && {ExclusiveStartKey: startKey}
    });
  }

  _getMonthKey(restoredKey) {
    if (restoredKey) return restoredKey.month;
    const currentDate = this._dateProvider.getCurrentDate().toISOString();
    return currentDate.substr(0, utils.MONTH_LENGTH);
  }

  _buildQueryParams({monthKey, exclusiveStartKey}) {
    return Object.assign(
      {
        TableName: this._collectionName,
        IndexName: 'date',
        Limit: DEFAULT_SCAN_LIMIT,
        KeyConditionExpression: '#month = :m',
        ExpressionAttributeNames: {
          '#month': 'month',
          '#date': 'date'
        },
        ExpressionAttributeValues: {':m': monthKey},
        ScanIndexForward: false,
        ProjectionExpression: 'id, #date, doneThing, userId',
        Select: 'SPECIFIC_ATTRIBUTES'
      },
      exclusiveStartKey
    );
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
