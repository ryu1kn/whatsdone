
import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';

type UserIdRecord = {
  id: string;
  cognitoUserId: string;
};

export default class UserIdRepository {
  private _docClient: AWS.DynamoDB.DocumentClient;
  private _tableName: string;

  constructor(tableName: string) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._tableName = tableName;
  }

  async getCognitoUserId(id: string): Promise<string|undefined> {
    const params = {
      TableName: this._tableName,
      Key: {id}
    };
    const response = await this._docClient.get(params).promise();
    return (response.Item || {}).cognitoUserId;
  }

  async getByCognitoUserId(cognitoUserId: string): Promise<string|undefined> {
    const params = {
      TableName: this._tableName,
      IndexName: 'cognitoUserId',
      KeyConditionExpression: 'cognitoUserId = :hkey',
      ExpressionAttributeValues: {':hkey': cognitoUserId},
      ProjectionExpression: 'id'
    };
    const result = await this._docClient.query(params).promise();
    const item = result.Items![0];
    return item && item.id;
  }

}
