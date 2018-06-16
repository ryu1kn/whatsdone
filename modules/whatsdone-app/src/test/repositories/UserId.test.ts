import ServiceLocator from '../../lib/ServiceLocator';
import UserIdRepository from '../../lib/repositories/UserId';
import {expect} from '../helper/TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server UserIdRepository', () => {

  it('queries user id database with given id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClientForGet();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getCognitoUserId('USER_ID').then(() => {
      expect(dynamoDBDocumentClient.get).to.have.been.calledWith({
        TableName: 'USER_ID_TABLE_NAME',
        Key: {id: 'USER_ID'}
      });
    });
  });

  it('finds a cognito user id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClientForGet();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getCognitoUserId('USER_ID').then(result => {
      expect(result).to.eql('COGNITO_USER_ID');
    });
  });

  it('searches on user id database for old user id with cognito user id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClientForQuery();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getByCognitoUserId('COGNITO_USER_ID').then(() => {
      expect(dynamoDBDocumentClient.query).to.have.been.calledWith({
        TableName: 'USER_ID_TABLE_NAME',
        IndexName: 'cognitoUserId',
        KeyConditionExpression: 'cognitoUserId = :hkey',
        ExpressionAttributeValues: {':hkey': 'COGNITO_USER_ID'},
        ProjectionExpression: 'id'
      });
    });
  });

  it('finds a user id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClientForQuery();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getByCognitoUserId('COGNITO_USER_ID').then(result => {
      expect(result).to.eql('OLD_USER_ID');
    });
  });

  function createDynamoDBDocumentClientForGet() {
    return {
      get: sinon.stub().returns({
        promise: () => Promise.resolve({
          Item: {cognitoUserId: 'COGNITO_USER_ID'}
        })
      })
    };
  }

  function createDynamoDBDocumentClientForQuery() {
    return {
      query: sinon.stub().returns({
        promise: () => Promise.resolve({
          Items: [{id: 'OLD_USER_ID'}]
        })
      })
    };
  }

  function createUserIdRepository(dynamoDBDocumentClient) {
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient
    } as ServiceFactory);
    return new UserIdRepository({tableName: 'USER_ID_TABLE_NAME'});
  }

});
