
const AWS = require('aws-sdk');
const Uuid = require('uuid');

class ServiceFactory {

  constructor({env}) {
    this._env = env;
  }

  getDynamoDB() {
    this._dynamoDB = this._dynamoDB || new AWS.DynamoDB({region: this._env.DB_REGION});
    return this._dynamoDB;
  }

  getDynamoDBDocumentClient() {
    this._documentClient = this._documentClient ||
        new AWS.DynamoDB.DocumentClient({region: this._env.DB_REGION});
    return this._documentClient;
  }

  getDoneRepository() {
    return require('./repositories/Dones');
  }

  getUserRepository() {
    return require('./repositories/Users');
  }

  getUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

}

module.exports = ServiceFactory;
