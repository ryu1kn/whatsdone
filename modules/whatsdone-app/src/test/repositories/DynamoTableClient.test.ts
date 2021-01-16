import DynamoTableClient from '../../lib/repositories/DynamoTableClient';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {deepStrictEqual, doesNotReject} from 'assert';
import {awsSdkResponse} from '../helper/AwsHelper';
import * as td from 'testdouble';
import {DynamoDB} from 'aws-sdk';

describe('Server DynamoTableClient', () => {
  const dynamoDBDocumentClient = td.instance(DynamoDB.DocumentClient);

  td.when(dynamoDBDocumentClient.get({TableName: 'TABLE_NAME', Key: {id: 'ITEM_ID'}}))
    .thenReturn(awsSdkResponse({Item: 'ITEM'}));

  td.when(dynamoDBDocumentClient.put({
    TableName: 'TABLE_NAME',
    Item: {
      DATA: '..',
      id: 'UUID'
    }
  })).thenReturn(awsSdkResponse());

  td.when(dynamoDBDocumentClient.delete({
    TableName: 'TABLE_NAME',
    Key: {id: 'ITEM_ID'}
  })).thenReturn(awsSdkResponse());

  td.when(dynamoDBDocumentClient.update({
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
  })).thenReturn(awsSdkResponse());

  ServiceLocator.load({
    createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
    createUuidGenerator: () => ({generate: () => 'UUID'})
  } as ServiceFactory);
  const client = new DynamoTableClient('TABLE_NAME', 'id');

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
