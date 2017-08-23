
const ServiceLocator = require('../../../lib/ServiceLocator');
const DoneQueryHelper = require('../../../lib/repositories/done-helpers/query');

describe('Server DoneQueryHelper', () => {

  it('uses DynamoDB#query to get done items', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns({
        promise: () => Promise.resolve({Items: [{DATA: '..'}]})
      })
    };
    const dateProvider = {getCurrentDate: () => new Date('2017-08-01T07:26:27.574Z')};
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider
    });
    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(() => {
      expect(dynamoDBDocumentClient.query).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        IndexName: 'date',
        Limit: 20,
        KeyConditionExpression: '#month = :m and #date > :d',
        ExpressionAttributeNames: {
          '#month': 'month',
          '#date': 'date'
        },
        ExpressionAttributeValues: {
          ':m': '2017-08',
          ':d': '2017-08-01T07:26:27.574Z'
        },
        ScanIndexForward: false,
        ProjectionExpression: 'id, #date, doneThing, userId',
        Select: 'SPECIFIC_ATTRIBUTES'
      });
    });
  });

  it('returns items', () => {
    const dateProvider = {getCurrentDate: () => new Date()};
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns({
        promise: () => Promise.resolve({Items: [{DATA: '..'}]})
      })
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider
    });
    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(result => {
      expect(result.items).to.eql([{DATA: '..'}]);
    });
  });

  it('returns items honouring next page key', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns({
        promise: () => Promise.resolve({Items: []})
      })
    };
    const dateProvider = {getCurrentDate: () => new Date()};
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider
    });
    const client = new DoneQueryHelper('TABLE_NAME');
    const nextKey = JSON.stringify({
      date: '2017-08-01T07:26:27.574Z',
      id: 'ID'
    });
    return client.query(nextKey).then(() => {
      const queryArgs = dynamoDBDocumentClient.query.args[0][0];
      expect(queryArgs.ExpressionAttributeValues).to.eql({
        ':m': '2017-08',
        ':d': '2017-08-01T07:26:27.574Z'
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
      query: sinon.stub().returns({
        promise: () => Promise.resolve({
          Items: [],
          LastEvaluatedKey: {
            id: 'ID',
            date: '2017-08-01T07:26:27.574Z',
            month: '2017-08'
          }
        })
      })
    };
    const dateProvider = {getCurrentDate: () => new Date()};
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createDateProvider: () => dateProvider
    });
    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(result => {
      expect(result.nextKey).to.eql('{"id":"ID","date":"2017-08-01T07:26:27.574Z"}');
    });
  });

});
