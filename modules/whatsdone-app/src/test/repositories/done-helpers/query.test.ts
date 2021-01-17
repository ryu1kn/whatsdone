import ServiceLocator from '../../../lib/ServiceLocator';
import DoneQueryHelper from '../../../lib/repositories/done-helpers/query';
import ServiceFactory from '../../../lib/ServiceFactory';
import * as td from 'testdouble';
import {Logger} from '../../../lib/Logger';
import {deepStrictEqual} from 'assert';
import {awsSdkResponse} from '../../helper/AwsHelper';
import {DynamoDB} from 'aws-sdk';

describe('Server DoneQueryHelper', () => {
  const padMonth = (n: number) => n < 10 ? `0${n}` : n;
  const prevMonth = (yearMonth: string) => {
    const [year, month] = yearMonth.split('-').map(n => parseInt(n, 10));
    return month > 1 ? `${year}-${padMonth(month - 1)}` : `${year - 1}-12`;
  };

  const queryWithoutNextKey = {
    TableName: 'TABLE_NAME',
    IndexName: 'date',
    Limit: 20,
    KeyConditionExpression: '#month = :m',
    ExpressionAttributeNames: {
      '#month': 'month',
      '#date': 'date'
    },
    ExpressionAttributeValues: {
      ':m': '2017-08'
    },
    ScanIndexForward: false,
    ProjectionExpression: 'id, #date, doneThing, userId',
    Select: 'SPECIFIC_ATTRIBUTES'
  };
  const makeItems = (size: number, group = 1) => [...Array(size)].map((_, i) => ({DATA: `ITEM-${group}-${i}`}));

  it('queries 20 items and that\'s everything', async () => {
    const items = makeItems(20);
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    td.when(dynamoDBDocumentClient.query(queryWithoutNextKey)).thenReturn(awsSdkResponse({Items: items}));
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    deepStrictEqual(await client.query(), {items, nextKey: undefined});
  });

  it('returns items honouring next page key', async () => {
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    td.when(dynamoDBDocumentClient.query({
      ...queryWithoutNextKey,
      ExclusiveStartKey: {
        id: 'ID',
        date: '2017-08-01T07:26:27.574Z',
        month: '2017-08'
      }
    })).thenReturn(awsSdkResponse({Items: makeItems(20)}));

    setupServiceLocator({dynamoDBDocumentClient});
    const client = new DoneQueryHelper('TABLE_NAME');

    const nextKey = JSON.stringify({
      date: '2017-08-01T07:26:27.574Z',
      id: 'ID'
    });
    const result = await client.query(nextKey);

    deepStrictEqual(result.items, makeItems(20));
  });

  it('returns a key for next page if it exists', async () => {
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    td.when(dynamoDBDocumentClient.query(queryWithoutNextKey)).thenReturn(awsSdkResponse({
      Items: makeItems(23),
      LastEvaluatedKey: {
        id: 'ID',
        date: '2017-08-02T07:26:27.574Z',
        month: '2017-08'
      }
    }));
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    const result = await client.query();
    deepStrictEqual(result.nextKey, '{"id":"ID","date":"2017-08-02T07:26:27.574Z"}');
  });

  it('automatically queries previous month if result does not have enough records', async () => {
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    td.when(dynamoDBDocumentClient.query(queryWithoutNextKey))
      .thenReturn(awsSdkResponse({Items: makeItems(17, 1)}));
    td.when(dynamoDBDocumentClient.query({...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-07'}, Limit: 3}))
      .thenReturn(awsSdkResponse({Items: makeItems(3, 2)}));
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');

    deepStrictEqual(await client.query(), {
      items: [...makeItems(17, 1), ...makeItems(3, 2)],
      nextKey: undefined
    });
  });

  it('automatically queries for previous 2 months if result does not have enough records', async () => {
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    td.when(dynamoDBDocumentClient.query(queryWithoutNextKey))
      .thenReturn(awsSdkResponse({Items: makeItems(15, 1)}));
    td.when(dynamoDBDocumentClient.query({...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-07'}, Limit: 5}))
      .thenReturn(awsSdkResponse({Items: makeItems(3, 2)}));
    td.when(dynamoDBDocumentClient.query({...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-06'}, Limit: 2}))
      .thenReturn(awsSdkResponse({Items: makeItems(2, 3)}));
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');

    deepStrictEqual(await client.query(), {
      items: [...makeItems(15, 1), ...makeItems(3, 2), ...makeItems(2, 3)],
      nextKey: undefined
    });
  });

  // XXX: This rule causes the very slow response time when it queries the DB 30 times!
  it('tries to find items as old as March 2015', async () => {
    const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);
    for (let yearMonth = '2017-08'; yearMonth !== '2015-02'; yearMonth = prevMonth(yearMonth)) {
      td.when(dynamoDBDocumentClient.query(td.matchers.contains({ExpressionAttributeValues: {':m': yearMonth}})))
        .thenReturn(awsSdkResponse({Items: [], LastEvaluatedKey: {date: `${yearMonth}-01T07:26:27.574Z`}}));
    }
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();

    deepStrictEqual(await client.query(), {
      items: [],
      nextKey: '{"date":"2015-03-01T07:26:27.574Z"}'
    });
  });

  function setupServiceLocator({dynamoDBDocumentClient, currentDate}: any) {
    const dateProvider = {
      getCurrentDate: () => currentDate ? new Date(currentDate) : new Date()
    };
    const logger = {error: () => {}} as Logger;
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider,
      createLogger: () => logger
    } as ServiceFactory);
  }
});
