
import ServiceLocator = require('../../../lib/ServiceLocator');
import DoneQueryHelper = require('../../../lib/repositories/done-helpers/query');
import {expect} from '../../TestUtils';
import sinon = require('sinon');

describe('Server DoneQueryHelper', () => {

  it('uses DynamoDB#query to get done items', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: [{DATA: '..'}]}))
    };
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query).to.have.been.calledWith({
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
      });
    });
  });

  it('returns items', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: [{DATA: '..'}]}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(result => {
      expect(result.items[0]).to.eql({DATA: '..'});
    });
  });

  it('returns items honouring next page key', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: []}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    const nextKey = JSON.stringify({
      date: '2017-08-01T07:26:27.574Z',
      id: 'ID'
    });
    return client.query(nextKey).then(() => {
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
  });

  it('returns a key for next page if it exists', () => {
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
    return client.query().then(result => {
      expect(result.nextKey).to.eql('{"id":"ID","date":"2017-08-01T07:26:27.574Z"}');
    });
  });

  it('automatically queries next month if result does not have enough records', () => {
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
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query.args[0][0].ExpressionAttributeValues)
        .to.eql({':m': '2017-08'});
      expect(dynamoDBDocumentClient.query.args[1][0].ExpressionAttributeValues)
        .to.eql({':m': '2017-07'});
    });
  });

  it('automatically queries for next 2 months if result does not have enough records', () => {
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
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query).to.have.been.calledThrice; // tslint:disable-line:no-unused-expression
      expect(dynamoDBDocumentClient.query.args[0][0].ExpressionAttributeValues)
        .to.eql({':m': '2017-08'});
      expect(dynamoDBDocumentClient.query.args[1][0].ExpressionAttributeValues)
        .to.eql({':m': '2017-07'});
      expect(dynamoDBDocumentClient.query.args[2][0].ExpressionAttributeValues)
        .to.eql({':m': '2017-06'});
    });
  });

  it('automatically fetches the number of items that satisfies original fetch limit', () => {
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
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query.args[0][0].Limit).to.eql(20);
      expect(dynamoDBDocumentClient.query.args[1][0].Limit).to.eql(3);
    });
  });

  it('tries to find items as old as March 2015', () => {
    const dynamoDBDocumentClient = {query: sinon.stub().returns(awsSdkResponse({Items: []}))};
    setupServiceLocator({dynamoDBDocumentClient, currentDate: '2017-08-01T07:26:27.574Z'});

    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query).to.have.callCount(30);
      expect(dynamoDBDocumentClient.query.args[29][0].ExpressionAttributeValues)
        .to.eql({':m': '2015-03'});
    });
  });

  it('does not return a key for next page if it does not exist', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: []}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(result => {
      expect(result.nextKey).to.be.undefined; // tslint:disable-line:no-unused-expression
    });
  });

  function setupServiceLocator({dynamoDBDocumentClient, currentDate}: any) {
    const dateProvider = {
      getCurrentDate: () => currentDate ? new Date(currentDate) : new Date()
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider,
      createLogger: () => ({log: () => {}})
    });
  }

  function awsSdkResponse(response) {
    const finalResponse = response instanceof Error ?
      Promise.reject(response) : Promise.resolve(response);
    return {promise: () => finalResponse};
  }

});
