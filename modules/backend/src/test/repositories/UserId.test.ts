import ServiceLocator from '../../lib/ServiceLocator';
import UserIdRepository from '../../lib/repositories/UserId';
import ServiceFactory from '../../lib/ServiceFactory';
import {DynamoDBClient} from '@aws-sdk/client-dynamodb';
import {DynamoDBDocumentClient, GetCommand, QueryCommand} from '@aws-sdk/lib-dynamodb';
import {AwsClientStub, mockClient} from 'aws-sdk-client-mock';
import {deepStrictEqual} from 'assert';

describe('Server UserIdRepository', () => {
  let ddbMock: AwsClientStub<DynamoDBDocumentClient>;
  let repository: UserIdRepository;

  beforeEach(() => {
    ddbMock = mockClient(DynamoDBDocumentClient);
    ddbMock.on(GetCommand, {
      TableName: 'USER_ID_TABLE_NAME',
      Key: {id: 'USER_ID'}
    }).resolves({
      Item: {cognitoUserId: 'COGNITO_USER_ID'}
    });
    ddbMock.on(QueryCommand, {
      TableName: 'USER_ID_TABLE_NAME',
      IndexName: 'cognitoUserId',
      KeyConditionExpression: 'cognitoUserId = :hkey',
      ExpressionAttributeValues: {':hkey': 'COGNITO_USER_ID'},
      ProjectionExpression: 'id'
    }).resolves({
      Items: [{id: 'OLD_USER_ID'}]
    });

    ServiceLocator.load({
      createDynamoDBDocumentClient: () => DynamoDBDocumentClient.from(new DynamoDBClient({region: 'ap-southeast-2'}))
    } as ServiceFactory);
    repository = new UserIdRepository('USER_ID_TABLE_NAME');
  });

  afterEach(() => ddbMock.restore());

  it('finds a cognito user id', async () => {
    const result = await repository.getCognitoUserId('USER_ID');

    deepStrictEqual(result, 'COGNITO_USER_ID');
  });

  it('finds a user id', async () => {
    const result = await repository.getByCognitoUserId('COGNITO_USER_ID');

    deepStrictEqual(result, 'OLD_USER_ID');
  });
});
