import ServiceLocator from '../../../lib/ServiceLocator';
import DoneQueryHelper from '../../../lib/repositories/done-helpers/query';
import ServiceFactory from '../../../lib/ServiceFactory';
import {Logger} from '../../../lib/Logger';
import {deepStrictEqual} from 'assert';
import {DynamoDBClient} from '@aws-sdk/client-dynamodb';
import {DynamoDBDocumentClient, QueryCommand} from '@aws-sdk/lib-dynamodb';
import {AwsClientStub, mockClient} from 'aws-sdk-client-mock';

describe('Server DoneQueryHelper', () => {
  let ddbMock: AwsClientStub<DynamoDBDocumentClient>;

  beforeEach(() => {
    ddbMock = mockClient(DynamoDBDocumentClient);
  });

  afterEach(() => ddbMock.restore());

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
    ProjectionExpression: 'id, #date, doneThing, userId, topics',
    Select: 'SPECIFIC_ATTRIBUTES' as const
  };
  const makeItems = (size: number, group = 1) => [...Array(size)].map((_, i) => ({DATA: `ITEM-${group}-${i}`}));

  it('queries 20 items and that\'s everything', async () => {
    const items = makeItems(20);
    ddbMock.on(QueryCommand, queryWithoutNextKey).resolves({Items: items});
    setupServiceLocator({currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    deepStrictEqual(await client.query(), {items, nextKey: undefined});
  });

  it('returns items honouring next page key', async () => {
    ddbMock.on(QueryCommand, {
      ...queryWithoutNextKey,
      ExclusiveStartKey: {
        id: 'ID',
        date: '2017-08-01T07:26:27.574Z',
        month: '2017-08'
      }
    }).resolves({Items: makeItems(20)});

    setupServiceLocator({});
    const client = new DoneQueryHelper('TABLE_NAME');

    const nextKey = JSON.stringify({
      date: '2017-08-01T07:26:27.574Z',
      id: 'ID'
    });
    const result = await client.query(nextKey);

    deepStrictEqual(result.items, makeItems(20));
  });

  it('returns a key for next page if it exists', async () => {
    ddbMock.on(QueryCommand, queryWithoutNextKey).resolves({
      Items: makeItems(23),
      LastEvaluatedKey: {
        id: 'ID',
        date: '2017-08-02T07:26:27.574Z',
        month: '2017-08'
      }
    });
    setupServiceLocator({currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    const result = await client.query();
    deepStrictEqual(result.nextKey, '{"id":"ID","date":"2017-08-02T07:26:27.574Z"}');
  });

  it('automatically queries previous month if result does not have enough records', async () => {
    ddbMock.on(QueryCommand, queryWithoutNextKey)
      .resolves({Items: makeItems(17, 1)});
    ddbMock.on(QueryCommand, {...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-07'}, Limit: 3})
      .resolves({Items: makeItems(3, 2)});
    setupServiceLocator({currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');

    deepStrictEqual(await client.query(), {
      items: [...makeItems(17, 1), ...makeItems(3, 2)],
      nextKey: undefined
    });
  });

  it('automatically queries for previous 2 months if result does not have enough records', async () => {
    ddbMock.on(QueryCommand, queryWithoutNextKey)
      .resolves({Items: makeItems(15, 1)});
    ddbMock.on(QueryCommand, {...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-07'}, Limit: 5})
      .resolves({Items: makeItems(3, 2)});
    ddbMock.on(QueryCommand, {...queryWithoutNextKey, ExpressionAttributeValues: {':m': '2017-06'}, Limit: 2})
      .resolves({Items: makeItems(2, 3)});
    setupServiceLocator({currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');

    deepStrictEqual(await client.query(), {
      items: [...makeItems(15, 1), ...makeItems(3, 2), ...makeItems(2, 3)],
      nextKey: undefined
    });
  });

  // XXX: This rule causes the very slow response time when it queries the DB 30 times!
  it('tries to find items as old as March 2015', async () => {
    for (let yearMonth = '2017-08'; yearMonth !== '2015-02'; yearMonth = prevMonth(yearMonth)) {
      ddbMock.on(QueryCommand, {ExpressionAttributeValues: {':m': yearMonth}})
        .resolves({Items: [], LastEvaluatedKey: {date: `${yearMonth}-01T07:26:27.574Z`}});
    }
    setupServiceLocator({currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();

    deepStrictEqual(await client.query(), {
      items: [],
      nextKey: '{"date":"2015-03-01T07:26:27.574Z"}'
    });
  });

  function setupServiceLocator({currentDate}: any) {
    const dateProvider = {
      getCurrentDate: () => currentDate ? new Date(currentDate) : new Date()
    };
    const logger = {error: () => {}} as Logger;
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => DynamoDBDocumentClient.from(new DynamoDBClient({region: 'ap-southeast-2'})),
      createDateProvider: () => dateProvider,
      createLogger: () => logger
    } as ServiceFactory);
  }
});
