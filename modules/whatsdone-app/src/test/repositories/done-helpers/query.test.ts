import ServiceLocator from '../../../lib/ServiceLocator';
import DoneQueryHelper from '../../../lib/repositories/done-helpers/query';
import {expect} from 'chai';
import ServiceFactory from '../../../lib/ServiceFactory';
import * as td from 'testdouble';
import {Logger} from '../../../lib/Logger';
import {deepStrictEqual} from 'assert';
import sinon = require('sinon');

describe('Server DoneQueryHelper', () => {

  it('uses DynamoDB#query to get done items', async () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: [{DATA: '..'}]}))
    };
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();
    expect(dynamoDBDocumentClient.query.args[0]).to.eql([{
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
    }]);
  });

  it('returns items', async () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: [{DATA: '..'}]}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    const result = await client.query();
    deepStrictEqual(result.items[0], {DATA: '..'});
  });

  it('returns items honouring next page key', async () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: []}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    const nextKey = JSON.stringify({
      date: '2017-08-01T07:26:27.574Z',
      id: 'ID'
    });
    await client.query(nextKey);
    const queryArgs = dynamoDBDocumentClient.query.args[0][0];
    expect(queryArgs.ExpressionAttributeValues).to.eql({
      ':m': '2017-08'
    });
    expect(queryArgs.ExclusiveStartKey).to.eql({
      id: 'ID',
      date: '2017-08-01T07:26:27.574Z',
      month: '2017-08'
    });
  });

  it('returns a key for next page if it exists', async () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({
        Items: [],
        LastEvaluatedKey: {
          id: 'ID',
          date: '2017-08-01T07:26:27.574Z',
          month: '2017-08'
        }
      }))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    const result = await client.query();
    deepStrictEqual(result.nextKey, '{"id":"ID","date":"2017-08-01T07:26:27.574Z"}');
  });

  it('automatically queries next month if result does not have enough records', async () => {
    const queryStub = sinon.stub();
    queryStub.onCall(0).returns(awsSdkResponse({
      Items: '.'.repeat(17).split('').map((v, i) => `ITEM-1-${i}`)
    }));
    queryStub.onCall(1).returns(awsSdkResponse({
      Items: '.'.repeat(3).split('').map((v, i) => `ITEM-2-${i}`)
    }));
    const dynamoDBDocumentClient = {query: queryStub};
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();
    expect(dynamoDBDocumentClient.query.args[0][0].ExpressionAttributeValues)
      .to.eql({':m': '2017-08'});
    expect(dynamoDBDocumentClient.query.args[1][0].ExpressionAttributeValues)
      .to.eql({':m': '2017-07'});
  });

  it('automatically queries for next 2 months if result does not have enough records', async () => {
    const queryStub = sinon.stub();
    queryStub.onCall(0).returns(awsSdkResponse({
      Items: '.'.repeat(15).split('').map((v, i) => `ITEM-1-${i}`)
    }));
    queryStub.onCall(1).returns(awsSdkResponse({
      Items: '.'.repeat(3).split('').map((v, i) => `ITEM-2-${i}`)
    }));
    queryStub.onCall(2).returns(awsSdkResponse({
      Items: '.'.repeat(2).split('').map((v, i) => `ITEM-3-${i}`)
    }));
    const dynamoDBDocumentClient = {query: queryStub};
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();
    deepStrictEqual(dynamoDBDocumentClient.query.args.length, 3); // tslint:disable-line:no-unused-expression
    expect(dynamoDBDocumentClient.query.args[0][0].ExpressionAttributeValues)
      .to.eql({':m': '2017-08'});
    expect(dynamoDBDocumentClient.query.args[1][0].ExpressionAttributeValues)
      .to.eql({':m': '2017-07'});
    expect(dynamoDBDocumentClient.query.args[2][0].ExpressionAttributeValues)
      .to.eql({':m': '2017-06'});
  });

  it('automatically fetches the number of items that satisfies original fetch limit', async () => {
    const queryStub = sinon.stub();
    queryStub.onCall(0).returns(awsSdkResponse({
      Items: '.'.repeat(17).split('').map((v, i) => `ITEM-1-${i}`)
    }));
    queryStub.onCall(1).returns(awsSdkResponse({
      Items: '.'.repeat(3).split('').map((v, i) => `ITEM-2-${i}`)
    }));
    const dynamoDBDocumentClient = {query: queryStub};
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();
    deepStrictEqual(dynamoDBDocumentClient.query.args[0][0].Limit, 20);
    deepStrictEqual(dynamoDBDocumentClient.query.args[1][0].Limit, 3);
  });

  it('tries to find items as old as March 2015', async () => {
    const dynamoDBDocumentClient = {query: sinon.stub().returns(awsSdkResponse({Items: []}))};
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    await client.query();
    deepStrictEqual(dynamoDBDocumentClient.query.args.length, 30);
    expect(dynamoDBDocumentClient.query.args[29][0].ExpressionAttributeValues)
      .to.eql({':m': '2015-03'});
  });

  it('does not return a key for next page if it does not exist', async () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: []}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    const result = await client.query();
    expect(result.nextKey).to.be.undefined; // tslint:disable-line:no-unused-expression
  });

  function setupServiceLocator({dynamoDBDocumentClient, currentDate}: any) {
    const dateProvider = {
      getCurrentDate: () => currentDate ? new Date(currentDate) : new Date()
    };
    const factory = td.object(['createDynamoDBDocumentClient', 'createDateProvider', 'createLogger']) as ServiceFactory;
    td.when(factory.createDynamoDBDocumentClient()).thenReturn(dynamoDBDocumentClient);
    td.when(factory.createDateProvider()).thenReturn(dateProvider);
    td.when(factory.createLogger()).thenReturn({error: () => {}} as Logger);
    ServiceLocator.load(factory);
  }

  function awsSdkResponse(response: any) {
    const finalResponse = response instanceof Error ?
      Promise.reject(response) : Promise.resolve(response);
    return {promise: () => finalResponse};
  }

});
