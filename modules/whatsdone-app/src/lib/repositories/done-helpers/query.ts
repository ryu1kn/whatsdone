
import _ = require('lodash');
import AWS = require('aws-sdk');
import ServiceLocator from '../../ServiceLocator';
import WrappedError from '../../WrappedError';
import utils = require('../utils');

const DEFAULT_SCAN_LIMIT = 20;
const OLDEST_QUERY_MONTH = '2015-02';

export default class DoneQueryHelper {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _dateProvider: {getCurrentDate: () => Date};
  private _collectionName: string;

  constructor(collectionName) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._dateProvider = ServiceLocator.dateProvider;
    this._collectionName = collectionName;
  }

  async query(nextKey?) {
    try {
      return await this._query(this._decodeNextKey(nextKey));
    } catch (e) {
      throw new WrappedError(e);
    }
  }

  async _query(startKey) {
    const queryUntil = async (params, accumulatedResponse) => {
      const queryResult = await this._docClient.query(params).promise();
      const newAccumulatedResponse = {
        Items: [...accumulatedResponse.Items, ...queryResult.Items],
        LastEvaluatedKey: queryResult.LastEvaluatedKey
      };
      if (newAccumulatedResponse.Items.length >= DEFAULT_SCAN_LIMIT) return newAccumulatedResponse;

      const prevMonthKey = this._getPrevMonthKey(params);
      if (prevMonthKey === OLDEST_QUERY_MONTH) return newAccumulatedResponse;

      const nextParams = this._buildQueryParams({
        monthKey: prevMonthKey,
        limit: DEFAULT_SCAN_LIMIT - newAccumulatedResponse.Items.length
      });
      return queryUntil(nextParams, newAccumulatedResponse);
    };
    const params = this._buildQueryFromStartKey(startKey);
    const response = await queryUntil(params, {Items: []});
    return this._buildResponse(response);
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

  _buildQueryParams({monthKey, exclusiveStartKey, limit}: any) {
    return Object.assign(
      {
        TableName: this._collectionName,
        IndexName: 'date',
        Limit: limit || DEFAULT_SCAN_LIMIT,
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
