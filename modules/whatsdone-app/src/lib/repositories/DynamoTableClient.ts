
import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';
import WrappedError from '../WrappedError';
import {DoneInDb} from '../models/Done';

export default class DynamoTableClient {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _uuidGenerator: {generate: () => string};
  private _collectionName: string;
  private _idName: string;

  constructor({collectionName, idName}) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._uuidGenerator = ServiceLocator.uuidGenerator;
    this._collectionName = collectionName;
    this._idName = idName;
  }

  async getById(id: string): Promise<DoneInDb> {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id)
    };
    try {
      const response = await this._docClient.get(params).promise();
      return response.Item as DoneInDb;
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async put(newData) {
    const id = this._uuidGenerator.generate();
    const params = {
      TableName: this.getTableName(),
      Item: Object.assign({}, newData, this.toIdObject(id))
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
      TableName: this.getTableName(),
      Key: this.toIdObject(id)
    };
    try {
      return await this._docClient.delete(params).promise();
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async update(id, newData) {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id),
      AttributeUpdates: this.getAttributeUpdatesValues(newData)
    };
    try {
      await this._docClient.update(params).promise();
      return await this.getById(id); // XXX: Don't query again
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  private toIdObject(id: string) {
    return {[this._idName]: id};
  }

  private getTableName() {
    return this._collectionName;
  }

  private getAttributeUpdatesValues(newData) {
    return Object.keys(newData).reduce((result, key) => {
      result[key] = {
        Action: 'PUT',
        Value: newData[key]
      };
      return result;
    }, {});
  }

}
