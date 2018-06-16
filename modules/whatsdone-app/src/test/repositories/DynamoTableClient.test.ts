import DynamoTableClient from '../../lib/repositories/DynamoTableClient';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../helper/TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server DynamoTableClient', () => {

  it('finds one item by ID', () => {
    const dynamoDBDocumentClient = {
      get: sinon.stub().returns({
        promise: () => Promise.resolve({Item: 'ITEM'})
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient({collectionName: 'TABLE_NAME', idName: 'id'});
    return client.getById('ITEM_ID').then(item => {
      expect(item).to.eql('ITEM');
      expect(dynamoDBDocumentClient.get).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        Key: {id: 'ITEM_ID'}
      });
    });
  });

  it('stores a new item', () => {
    const dynamoDBDocumentClient = {
      put: sinon.stub().returns({
        promise: () => Promise.resolve()
      })
    };
    initialiseServiceLocator({
      dynamoDBDocumentClient,
      uuidGenerator: {generate: () => 'UUID'}
    });
    const client = new DynamoTableClient({collectionName: 'TABLE_NAME', idName: 'id'});
    const newItem = {DATA: '..'};
    return client.put(newItem).then(newId => {
      expect(newId).to.eql('UUID');
      expect(dynamoDBDocumentClient.put).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        Item: {
          DATA: '..',
          id: 'UUID'
        }
      });
    });
  });

  it('deletes one item', () => {
    const dynamoDBDocumentClient = {
      delete: sinon.stub().returns({
        promise: () => Promise.resolve()
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient({collectionName: 'TABLE_NAME', idName: 'id'});
    return client.delete('ITEM_ID').then(() => {
      expect(dynamoDBDocumentClient.delete).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        Key: {id: 'ITEM_ID'}
      });
    });
  });

  it('updates one item', () => {
    const dynamoDBDocumentClient = {
      update: sinon.stub().returns({
        promise: () => Promise.resolve()
      }),
      get: sinon.stub().returns({
        promise: () => Promise.resolve({Item: 'ITEM'})
      })
    };
    initialiseServiceLocator({dynamoDBDocumentClient});
    const client = new DynamoTableClient({collectionName: 'TABLE_NAME', idName: 'id'});
    const newData = {KEY_1: 'VALUE_1', KEY_2: 'VALUE_2'};
    return client.update('ITEM_ID', newData).then(item => {
      expect(item).to.eql('ITEM');
      expect(dynamoDBDocumentClient.update).to.have.been.calledWith({
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
      });
      expect(dynamoDBDocumentClient.get).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        Key: {id: 'ITEM_ID'}
      });
    });
  });

  function initialiseServiceLocator({dynamoDBDocumentClient = {}, uuidGenerator = {}} = {}) {
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => uuidGenerator
    } as ServiceFactory);
  }
});
