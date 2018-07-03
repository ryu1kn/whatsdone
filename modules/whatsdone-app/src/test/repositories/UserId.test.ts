import ServiceLocator from '../../lib/ServiceLocator';
import UserIdRepository from '../../lib/repositories/UserId';
import {expect} from 'chai';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';

describe('Server UserIdRepository', () => {

  it('finds a cognito user id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClient();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getCognitoUserId('USER_ID').then(result => {
      expect(result).to.eql('COGNITO_USER_ID');
    });
  });

  it('finds a user id', () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClient();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    return repository.getByCognitoUserId('COGNITO_USER_ID').then(result => {
      expect(result).to.eql('OLD_USER_ID');
    });
  });

  function createDynamoDBDocumentClient(): AWS.DynamoDB.DocumentClient {
    const docClient = td.object('get') as AWS.DynamoDB.DocumentClient;
    td.when(docClient.get({
      TableName: 'USER_ID_TABLE_NAME',
      Key: {id: 'USER_ID'}
    })).thenReturn({
      promise: () => Promise.resolve({
        Item: {cognitoUserId: 'COGNITO_USER_ID'}
      })
    });
    td.when(docClient.query({
      TableName: 'USER_ID_TABLE_NAME',
      IndexName: 'cognitoUserId',
      KeyConditionExpression: 'cognitoUserId = :hkey',
      ExpressionAttributeValues: {':hkey': 'COGNITO_USER_ID'},
      ProjectionExpression: 'id'
    })).thenReturn({
      promise: () => Promise.resolve({
        Items: [{id: 'OLD_USER_ID'}]
      })
    });
    return docClient;
  }

  function createUserIdRepository(dynamoDBDocumentClient: AWS.DynamoDB.DocumentClient) {
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient
    } as ServiceFactory);
    return new UserIdRepository('USER_ID_TABLE_NAME');
  }

});
