
import _ = require('lodash');
import AWS = require('aws-sdk');
import ServiceLocator from '../../ServiceLocator';
import WrappedError from '../../WrappedError';
import * as utils from '../utils';

const DEFAULT_SCAN_LIMIT = 20;
const OLDEST_QUERY_MONTH = '2015-02';

type QueryParams = {
  monthKey: string,
  exclusiveStartKey?: {ExclusiveStartKey: string},
  limit?: number
};

export type DoneInDb = {
  id: string;
  date: string;
  doneThing: string;
  userId: string;
  month: string;
};

export type DoneQueryResult = {
  items: DoneInDb[];
  nextKey: string;
};

export default class DoneQueryHelper {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _dateProvider: {getCurrentDate: () => Date};
  private _collectionName: string;

  constructor(collectionName) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._dateProvider = ServiceLocator.dateProvider;
    this._collectionName = collectionName;
  }

  async query(nextKey?): Promise<DoneQueryResult> {
    try {
      return await this._query(this.decodeNextKey(nextKey));
    } catch (e) {
      throw new WrappedError(e);
    }
  }

  private async _query(startKey): Promise<DoneQueryResult> {
    const queryUntil = async (params, accumulatedResponse) => {
      const queryResult = await this._docClient.query(params).promise();
      const newAccumulatedResponse = {
        Items: [...accumulatedResponse.Items, ...queryResult.Items],
        LastEvaluatedKey: queryResult.LastEvaluatedKey
      };
      if (newAccumulatedResponse.Items.length >= DEFAULT_SCAN_LIMIT) return newAccumulatedResponse;

      const prevMonthKey = this.getPrevMonthKey(params);
      if (prevMonthKey === OLDEST_QUERY_MONTH) return newAccumulatedResponse;

      const nextParams = this.buildQueryParams({
        monthKey: prevMonthKey,
        limit: DEFAULT_SCAN_LIMIT - newAccumulatedResponse.Items.length
      });
      return queryUntil(nextParams, newAccumulatedResponse);
    };
    const params = this.buildQueryFromStartKey(startKey);
    const response = await queryUntil(params, {Items: []});
    return this.buildResponse(response);
  }

  private getPrevMonthKey(params) {
    function pad0(number) {
      return number < 10 ? `0${number}` : number;
    }
    const d = new Date(params.ExpressionAttributeValues[':m']);
    const oldMonth = d.getMonth() === 0 ? 12 : d.getMonth();
    const oldYear = d.getMonth() === 0 ? d.getFullYear() - 1 : d.getFullYear();
    return `${oldYear}-${pad0(oldMonth)}`;
  }

  private buildQueryFromStartKey(startKey) {
    return this.buildQueryParams({
      monthKey: this.getMonthKey(startKey),
      exclusiveStartKey: startKey && {ExclusiveStartKey: startKey}
    });
  }

  private getMonthKey(restoredKey) {
    if (restoredKey) return restoredKey.month;
    const currentDate = this._dateProvider.getCurrentDate().toISOString();
    return currentDate.substr(0, utils.MONTH_LENGTH);
  }

  private buildQueryParams({monthKey, exclusiveStartKey, limit}: QueryParams) {
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

  private decodeNextKey(nextKey) {
    return nextKey ? utils.getDoneWithMonth(JSON.parse(nextKey)) : null;
  }

  private encodeNextKey(keyObject) {
    return keyObject && JSON.stringify(_.omit(keyObject, 'month'));
  }

  private buildResponse(queryResult): DoneQueryResult {
    return {
      items: queryResult.Items.map(done => _.omit(done, 'month')),
      nextKey: this.encodeNextKey(queryResult.LastEvaluatedKey)
    };
  }

}
