
const DynamoTableClient = require('../../lib/repositories/DynamoTableClient');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server DynamoTableClient', () => {

  it('finds one item by ID', () => {
    const dynamoDBDocumentClient = {
      get: sinon.stub().returns({
        promise: () => Promise.resolve({Item: 'ITEM'})
      })
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
    return client.getById('ITEM_ID').then(item => {
      expect(item).to.eql('ITEM');
      expect(dynamoDBDocumentClient.get).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        Key: {id: 'ITEM_ID'}
      });
    });
  });

  it('finds items by IDs', () => {
    const dynamoDBDocumentClient = {
      batchGet: sinon.stub().returns({
        promise: () => Promise.resolve({
          Responses: {
            TABLE_NAME: ['ITEM_1', 'ITEM_2']
          }
        })
      })
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
    return client.getByIds(['ITEM_ID_1', 'ITEM_ID_2']).then(items => {
      expect(items).to.eql(['ITEM_1', 'ITEM_2']);
      expect(dynamoDBDocumentClient.batchGet).to.have.been.calledWith({
        RequestItems: {
          TABLE_NAME: {
            Keys: [{id: 'ITEM_ID_1'}, {id: 'ITEM_ID_2'}]
          }
        }
      });
    });
  });

  it('returns an empty list if ID list is empty', () => {
    const dynamoDBDocumentClient = {
      batchGet: sinon.spy()
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
    return client.getByIds([]).then(items => {
      expect(items).to.eql([]);
      expect(dynamoDBDocumentClient.batchGet).to.have.been.not.called;
    });
  });

  it('finds an item by matching condition', () => {
    const dynamoDBDocumentClient = {
      scan: sinon.stub().returns({
        promise: () => Promise.resolve({
          Items: ['ITEM']
        })
      })
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
    const query = {
      KEY_1: 'VALUE_1',
      KEY_2: 'VALUE_2'
    };
    return client.getByQuery(query).then(item => {
      expect(item).to.eql('ITEM');
      expect(dynamoDBDocumentClient.scan).to.have.been.calledWith({
        TableName: 'TABLE_NAME',
        ExpressionAttributeValues: {
          ':KEY_1': 'VALUE_1',
          ':KEY_2': 'VALUE_2'
        },
        FilterExpression: 'KEY_1 = :KEY_1 AND KEY_2 = :KEY_2'
      });
    });
  });

  it('stores a new item', () => {
    const dynamoDBDocumentClient = {
      put: sinon.stub().returns({
        promise: () => Promise.resolve()
      })
    };
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => ({generate: () => 'UUID'})
    });
    const client = new DynamoTableClient('TABLE_NAME');
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
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
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
    ServiceLocator.load({
      createDynamoDBDocumentClient: () => dynamoDBDocumentClient,
      createUuidGenerator: () => {}
    });
    const client = new DynamoTableClient('TABLE_NAME');
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

});
