
const ServiceLocator = require('../../../lib/ServiceLocator');
const DoneQueryHelper = require('../../../lib/repositories/done-helpers/query');

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
      expect(result.items).to.eql([{DATA: '..'}]);
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

  it('does not return a key for next page if it does not exist', () => {
    const dynamoDBDocumentClient = {
      query: sinon.stub().returns(awsSdkResponse({Items: []}))
    };
    setupServiceLocator({dynamoDBDocumentClient});

    const client = new DoneQueryHelper('TABLE_NAME');
    return client.query().then(result => {
      expect(result.nextKey).to.be.undefined;
    });
  });

  function setupServiceLocator({dynamoDBDocumentClient, currentDate}) {
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
