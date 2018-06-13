
import ServiceLocator from '../ServiceLocator';

class UserIdRepository {
  private _docClient: any;
  private _tableName: any;

  constructor({tableName}) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._tableName = tableName;
  }

  async getCognitoUserId(id) {
    const params = {
      TableName: this._tableName,
      Key: {id}
    };
    const response = await this._docClient.get(params).promise();
    return (response.Item || {}).cognitoUserId;
  }

  async getByCognitoUserId(cognitoUserId) {
    const params = {
      TableName: this._tableName,
      IndexName: 'cognitoUserId',
      KeyConditionExpression: 'cognitoUserId = :hkey',
      ExpressionAttributeValues: {':hkey': cognitoUserId},
      ProjectionExpression: 'id'
    };
    const result = await this._docClient.query(params).promise();
    const items = result.Items[0];
    return items && items.id;
  }

}

export = UserIdRepository;
