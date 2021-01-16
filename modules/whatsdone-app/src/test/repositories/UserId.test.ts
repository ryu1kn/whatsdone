import ServiceLocator from '../../lib/ServiceLocator';
import UserIdRepository from '../../lib/repositories/UserId';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import {PromiseResult} from 'aws-sdk/lib/request';
import {AWSError, DynamoDB} from 'aws-sdk';
import {GetItemOutput, QueryOutput} from 'aws-sdk/clients/dynamodb';
import {deepStrictEqual} from 'assert';

describe('Server UserIdRepository', () => {
  const awsResponse = (res: any) => ({promise: () => Promise.resolve(res)});

  const docClient = td.instance(DynamoDB.DocumentClient);
  td.when(docClient.get({
    TableName: 'USER_ID_TABLE_NAME',
    Key: {id: 'USER_ID'}
  })).thenReturn(awsResponse({
    Item: {cognitoUserId: 'COGNITO_USER_ID'}
  } as unknown as PromiseResult<GetItemOutput, AWSError>));
  td.when(docClient.query({
    TableName: 'USER_ID_TABLE_NAME',
    IndexName: 'cognitoUserId',
    KeyConditionExpression: 'cognitoUserId = :hkey',
    ExpressionAttributeValues: {':hkey': 'COGNITO_USER_ID'},
    ProjectionExpression: 'id'
  })).thenReturn(awsResponse({
    Items: [{id: 'OLD_USER_ID'}]
  } as unknown as PromiseResult<QueryOutput, AWSError>));

  ServiceLocator.load({createDynamoDBDocumentClient: () => docClient} as ServiceFactory);
  const repository = new UserIdRepository('USER_ID_TABLE_NAME');

  it('finds a cognito user id', async () => {
    const result = await repository.getCognitoUserId('USER_ID');

    deepStrictEqual(result, 'COGNITO_USER_ID');
  });

  it('finds a user id', async () => {
    const result = await repository.getByCognitoUserId('COGNITO_USER_ID');

    deepStrictEqual(result, 'OLD_USER_ID');
  });
});
