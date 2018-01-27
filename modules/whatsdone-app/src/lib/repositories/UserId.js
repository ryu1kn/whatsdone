
const ServiceLocator = require('../ServiceLocator');

class UserIdRepository {

  constructor({tableName}) {
    this._docClient = ServiceLocator.dynamoDBDocumentClient;
    this._tableName = tableName;
  }

  getCognitoUserId(id) {
    const params = {
      TableName: this._tableName,
      Key: {id}
    };
    return this._docClient.get(params).promise()
      .then(response => (response.Item || {}).cognitoUserId);
  }

  getByCognitoUserId(cognitoUserId) {
    const params = {
      TableName: this._tableName,
      IndexName: 'cognitoUserId',
      KeyConditionExpression: 'cognitoUserId = :hkey',
      ExpressionAttributeValues: {':hkey': cognitoUserId},
      ProjectionExpression: 'id'
    };
    return this._docClient.query(params).promise()
      .then(result => {
        const items = result.Items[0];
        return items && items.id;
      });
  }

}

module.exports = UserIdRepository;
