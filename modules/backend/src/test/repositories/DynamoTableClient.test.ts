import DynamoTableClient from '../../lib/repositories/DynamoTableClient';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {deepStrictEqual, doesNotReject} from 'assert';
import {DynamoDBClient} from '@aws-sdk/client-dynamodb';
import {DeleteCommand, DynamoDBDocumentClient, GetCommand, PutCommand, UpdateCommand} from '@aws-sdk/lib-dynamodb';
import {AwsClientStub, mockClient} from 'aws-sdk-client-mock';

describe('Server DynamoTableClient', () => {
  let ddbMock: AwsClientStub<DynamoDBDocumentClient>;
  let client: DynamoTableClient;

  beforeEach(() => {
    ddbMock = mockClient(DynamoDBDocumentClient);

    ddbMock.on(GetCommand, {TableName: 'TABLE_NAME', Key: {id: 'ITEM_ID'}})
      .resolves({Item: 'ITEM' as any});

    ddbMock.on(PutCommand, {
      TableName: 'TABLE_NAME',
      Item: {
        DATA: '..',
        id: 'UUID'
      }
    }).resolves({});

    ddbMock.on(DeleteCommand, {
      TableName: 'TABLE_NAME',
      Key: {id: 'ITEM_ID'}
    }).resolves({});

    ddbMock.on(UpdateCommand, {
      TableName: 'TABLE_NAME',
      Key: {id: 'ITEM_ID'},
      AttributeUpdates: {
        KEY_1: {
          Action: 'PUT',
          Value: 'VALUE_1'
        },
        KEY_2: {
          Action: 'PUT',
          Value: 'VALUE_2'
        }
      }
    }).resolves({});

    ServiceLocator.load({
      createDynamoDBDocumentClient: () => DynamoDBDocumentClient.from(new DynamoDBClient({region: 'ap-southeast-2'})),
      createUuidGenerator: () => ({generate: () => 'UUID'})
    } as ServiceFactory);
    client = new DynamoTableClient('TABLE_NAME', 'id');
  });

  afterEach(() => ddbMock.restore());

  it('finds one item by ID', async () => {
    const item = await client.getById('ITEM_ID');

    deepStrictEqual(item, 'ITEM');
  });

  it('stores a new item', async () => {
    const newId = await client.put({DATA: '..'});

    deepStrictEqual(newId, 'UUID');
  });

  it('deletes one item', async () => {
    await doesNotReject(client.delete('ITEM_ID'));
  });

  it('updates one item', async () => {
    const newData = {KEY_1: 'VALUE_1', KEY_2: 'VALUE_2'};

    const item = await client.update('ITEM_ID', newData);

    deepStrictEqual(item, 'ITEM');
  });
});
