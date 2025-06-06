import _omit = require('lodash.omit');
import AWS = require('aws-sdk');
import ServiceLocator from '../../ServiceLocator';
import WrappedError from '../../WrappedError';
import * as utils from '../utils';
import {DoneInDb} from '../../models/Done';

const DEFAULT_SCAN_LIMIT = 20;
const OLDEST_QUERY_MONTH = '2015-02';

type QueryParams = {
  monthKey: string,
  exclusiveStartKey?: {ExclusiveStartKey: DoneLastEvaluatedKey},
  limit?: number
};

export type DoneQueryResult = {
  items: DoneInDb[];
  nextKey?: string;
};

type DoneQueryParams = {
  TableName: string;
  IndexName: string;
  Limit: number;
  KeyConditionExpression: string;
  ExpressionAttributeNames: {
    '#month': string,
    '#date': string
  };
  ExpressionAttributeValues: {':m': string;};
  ScanIndexForward: boolean;
  ProjectionExpression: string;
  Select: string;
  ExclusiveStartKey?: DoneLastEvaluatedKey;
};

type DoneLastEvaluatedKey = {
  id: string;
  month: string;
  date: string;
};

type DynamoQueryResult = {
  Items: DoneInDb[];
  LastEvaluatedKey?: DoneLastEvaluatedKey;
};

export default class DoneQueryHelper {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _dateProvider: {getCurrentDate: () => Date};
  private _collectionName: string;

  constructor(collectionName: string) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._dateProvider = ServiceLocator.dateProvider;
    this._collectionName = collectionName;
  }

  async query(nextKey?: string): Promise<DoneQueryResult> {
    try {
      return await this._query(this.decodeNextKey(nextKey));
    } catch (e) {
      throw new WrappedError(e);
    }
  }

  private async _query(startKey?: DoneLastEvaluatedKey): Promise<DoneQueryResult> {
    const queryUntil = async (params: DoneQueryParams, accumulatedResponse: DynamoQueryResult): Promise<DynamoQueryResult> => {
      const queryResult = await this._docClient.query(params).promise();
      const newAccumulatedResponse = {
        Items: [...accumulatedResponse.Items, ...queryResult.Items as DoneInDb[]],
        LastEvaluatedKey: queryResult.LastEvaluatedKey as DoneLastEvaluatedKey
      };
      if (newAccumulatedResponse.Items.length >= DEFAULT_SCAN_LIMIT) return newAccumulatedResponse;

      const prevMonthKey = this.getPrevMonthKey(params);
      if (prevMonthKey === OLDEST_QUERY_MONTH) return newAccumulatedResponse;

      const nextParams = this.buildQueryParams({
        monthKey: prevMonthKey,
        limit: DEFAULT_SCAN_LIMIT - newAccumulatedResponse.Items.length
      });
      // TODO: Recursive calls don't use LastEvaluatedKey. Is this correct?
      return queryUntil(nextParams, newAccumulatedResponse);
    };
    const params = this.buildQueryFromStartKey(startKey);
    const response = await queryUntil(params, {Items: []});
    return this.buildResponse(response);
  }

  private getPrevMonthKey(params: DoneQueryParams) {
    function pad0(number: number) {
      return number < 10 ? `0${number}` : number;
    }
    const d = new Date(params.ExpressionAttributeValues[':m']);
    const oldMonth = d.getMonth() === 0 ? 12 : d.getMonth();
    const oldYear = d.getMonth() === 0 ? d.getFullYear() - 1 : d.getFullYear();
    return `${oldYear}-${pad0(oldMonth)}`;
  }

  private buildQueryFromStartKey(startKey?: DoneLastEvaluatedKey) {
    return this.buildQueryParams({
      monthKey: this.getMonthKey(startKey),
      exclusiveStartKey: startKey && {ExclusiveStartKey: startKey}
    });
  }

  private getMonthKey(restoredKey?: DoneLastEvaluatedKey): string {
    if (restoredKey) return restoredKey.month;
    const currentDate = this._dateProvider.getCurrentDate().toISOString();
    return currentDate.substr(0, utils.MONTH_LENGTH);
  }

  private buildQueryParams({monthKey, exclusiveStartKey, limit}: QueryParams): DoneQueryParams {
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
        ProjectionExpression: 'id, #date, doneThing, userId, topics',
        Select: 'SPECIFIC_ATTRIBUTES'
      },
      exclusiveStartKey
    );
  }

  private decodeNextKey(nextKey?: string): DoneLastEvaluatedKey | undefined {
    return nextKey
      ? utils.getDoneWithMonth(JSON.parse(nextKey)) as DoneLastEvaluatedKey
      : undefined;
  }

  private encodeNextKey(keyObject?: DoneLastEvaluatedKey) {
    return keyObject && JSON.stringify(_omit(keyObject, 'month'));
  }

  private buildResponse(queryResult: DynamoQueryResult): DoneQueryResult {
    return {
      items: queryResult.Items.map(done => _omit(done, 'month')),
      nextKey: this.encodeNextKey(queryResult.LastEvaluatedKey)
    };
  }

}
