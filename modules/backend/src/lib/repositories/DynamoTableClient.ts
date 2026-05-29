import {DeleteCommand, DynamoDBDocumentClient, GetCommand, PutCommand, UpdateCommand} from '@aws-sdk/lib-dynamodb';
import ServiceLocator from '../ServiceLocator';
import WrappedError from '../WrappedError';
import {ObjectMap} from '../models/Collection';

export default class DynamoTableClient {
  private _docClient: DynamoDBDocumentClient;
  private _uuidGenerator: {generate: () => string};
  private _collectionName: string;
  private _idName: string;

  constructor(collectionName: string, idName: string) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._uuidGenerator = ServiceLocator.uuidGenerator;
    this._collectionName = collectionName;
    this._idName = idName;
  }

  async getById(id: string): Promise<ObjectMap<any>> {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id)
    };
    try {
      const response = await this._docClient.send(new GetCommand(params));
      return response.Item as ObjectMap<any>;
    } catch (e) {
      throw new WrappedError(e as Error, params);
    }
  }

  async put(newData: ObjectMap<any>): Promise<string> {
    const id = this._uuidGenerator.generate();
    const params = {
      TableName: this.getTableName(),
      Item: Object.assign({}, newData, this.toIdObject(id))
    };
    try {
      await this._docClient.send(new PutCommand(params));
      return id;
    } catch (e) {
      throw new WrappedError(e as Error, params);
    }
  }

  async delete(id: string): Promise<void> {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id)
    };
    try {
      await this._docClient.send(new DeleteCommand(params));
    } catch (e) {
      throw new WrappedError(e as Error, params);
    }
  }

  async update(id: string, newData: ObjectMap<any>): Promise<ObjectMap<any>> {
    const params = {
      TableName: this.getTableName(),
      Key: this.toIdObject(id),
      AttributeUpdates: this.getAttributeUpdatesValues(newData)
    };
    try {
      await this._docClient.send(new UpdateCommand(params));
      return await this.getById(id); // XXX: Don't query again
    } catch (e) {
      throw new WrappedError(e as Error, params);
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
