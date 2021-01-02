import ServiceLocator from '../../lib/ServiceLocator';
import UserIdRepository from '../../lib/repositories/UserId';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import {PromiseResult} from 'aws-sdk/lib/request';
import {AWSError} from 'aws-sdk';
import {GetItemOutput, QueryOutput} from 'aws-sdk/clients/dynamodb';
import {deepStrictEqual} from 'assert';

describe('Server UserIdRepository', () => {

  it('finds a cognito user id', async () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClient();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    const result = await repository.getCognitoUserId('USER_ID');
    deepStrictEqual(result, 'COGNITO_USER_ID');
  });

  it('finds a user id', async () => {
    const dynamoDBDocumentClient = createDynamoDBDocumentClient();
    const repository = createUserIdRepository(dynamoDBDocumentClient);
    const result = await repository.getByCognitoUserId('COGNITO_USER_ID');
    deepStrictEqual(result, 'OLD_USER_ID');
  });

  function createDynamoDBDocumentClient(): AWS.DynamoDB.DocumentClient {
    const docClient = td.object('get') as AWS.DynamoDB.DocumentClient;
    td.when(docClient.get({
      TableName: 'USER_ID_TABLE_NAME',
      Key: {id: 'USER_ID'}
    })).thenReturn({
      promise: () => Promise.resolve({
        Item: {cognitoUserId: 'COGNITO_USER_ID'}
      } as unknown as PromiseResult<GetItemOutput, AWSError>)
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
      } as unknown as PromiseResult<QueryOutput, AWSError>)
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
