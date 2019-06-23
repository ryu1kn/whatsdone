import DynamoTableClient from '../../lib/repositories/DynamoTableClient';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from 'chai';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server DynamoTableClient', () => {

  it('finds one item by ID', async () => {
    const dynamoDBDocumentClient = {
      get: sinon.stub().returns({
        promise: () => Promise.resolve({Item: 'ITEM'})
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient('TABLE_NAME', 'id');
    const item = await client.getById('ITEM_ID');
    expect(item).to.eql('ITEM');
    expect(dynamoDBDocumentClient.get.args[0]).to.eql([{
      TableName: 'TABLE_NAME',
      Key: {id: 'ITEM_ID'}
    }]);
  });

  it('stores a new item', async () => {
    const dynamoDBDocumentClient = {
      put: sinon.stub().returns({
        promise: () => Promise.resolve()
      })
    };
    initialiseServiceLocator({
      dynamoDBDocumentClient,
      uuidGenerator: {generate: () => 'UUID'}
    });
    const client = new DynamoTableClient('TABLE_NAME', 'id');
    const newItem = {DATA: '..'};
    const newId = await client.put(newItem);
    expect(newId).to.eql('UUID');
    expect(dynamoDBDocumentClient.put.args[0]).to.eql([{
      TableName: 'TABLE_NAME',
      Item: {
        DATA: '..',
        id: 'UUID'
      }
    }]);
  });

  it('deletes one item', async () => {
    const dynamoDBDocumentClient = {
      delete: sinon.stub().returns({
        promise: () => Promise.resolve()
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient('TABLE_NAME', 'id');
    await client.delete('ITEM_ID');
    expect(dynamoDBDocumentClient.delete.args[0]).to.eql([{
      TableName: 'TABLE_NAME',
      Key: {id: 'ITEM_ID'}
    }]);
  });

  it('updates one item', async () => {
    const dynamoDBDocumentClient = {
      update: sinon.stub().returns({
        promise: () => Promise.resolve()
      }),
      get: sinon.stub().returns({
        promise: () => Promise.resolve({Item: 'ITEM'})
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient('TABLE_NAME', 'id');
    const newData = {KEY_1: 'VALUE_1', KEY_2: 'VALUE_2'};
    const item = await client.update('ITEM_ID', newData);
    expect(item).to.eql('ITEM');
    expect(dynamoDBDocumentClient.update.args[0]).to.eql([{
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
    }]);
    expect(dynamoDBDocumentClient.get.args[0]).to.eql([{
      TableName: 'TABLE_NAME',
      Key: {id: 'ITEM_ID'}
    }]);
  });

  function initialiseServiceLocator({dynamoDBDocumentClient = {}, uuidGenerator = {}} = {}) {
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => uuidGenerator
    } as ServiceFactory);
  }
});
