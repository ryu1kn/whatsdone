
import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';
import WrappedError from '../WrappedError';

class DynamoTableClient {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _uuidGenerator: any;
  private _collectionName: any;
  private _idName: any;

  constructor({collectionName, idName}) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._uuidGenerator = ServiceLocator.uuidGenerator;
    this._collectionName = collectionName;
    this._idName = idName;
  }

  async getById(id) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id)
    };
    try {
      const response = await this._docClient.get(params).promise();
      return response.Item;
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async put(newData) {
    const id = this._uuidGenerator.generate();
    const params = {
      TableName: this._getTableName(),
      Item: Object.assign({}, newData, this._toIdObject(id))
    };
    try {
      await this._docClient.put(params).promise();
      return id;
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async delete(id) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id)
    };
    try {
      return await this._docClient.delete(params).promise();
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async update(id, newData) {
    const params = {
      TableName: this._getTableName(),
      Key: this._toIdObject(id),
      AttributeUpdates: this._getAttributeUpdatesValues(newData)
    };
    try {
      await this._docClient.update(params).promise();
      return await this.getById(id); // XXX: Don't query again
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  _toIdObject(id) {
    return {[this._idName]: id};
  }

  _getTableName() {
    return this._collectionName;
  }

  _getAttributeUpdatesValues(newData) {
    return Object.keys(newData).reduce((result, key) => {
      result[key] = {
        Action: 'PUT',
        Value: newData[key]
      };
      return result;
    }, {});
  }

}

export = DynamoTableClient;
