import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';
import WrappedError from '../WrappedError';
import {DoneInDb} from '../models/Done';
import {ObjectMap} from '../models/Collection';

export default class DynamoTableClient {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _uuidGenerator: {generate: () => string};
  private _collectionName: string;
  private _idName: string;

  constructor(collectionName: string, idName: string) {
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

  async put(newData: ObjectMap<any>): Promise<string> {
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

  async delete(id: string): Promise<void> {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id)
    };
    try {
      await this._docClient.delete(params).promise();
    } catch (e) {
      throw new WrappedError(e, params);
    }
  }

  async update(id: string, newData: ObjectMap<any>): Promise<DoneInDb> {
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

  private getAttributeUpdatesValues(newData: ObjectMap<any>) {
    return Object.keys(newData).reduce((result: ObjectMap<any>, key) => {
      result[key] = {
        Action: 'PUT',
        Value: newData[key]
      };
      return result;
    }, {});
  }

}
