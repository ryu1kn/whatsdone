
const AWS = require('aws-sdk');
const Uuid = require('uuid');
const sha1 = require('sha1');

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

  getDoneDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient('dones');
  }

  getDoneRepository() {
    const DoneRepository = require('./repositories/Dones');
    return new DoneRepository();
  }

  getUserDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient('users');
  }

  getUserRepository() {
    const UserRepository = require('./repositories/Users');
    return new UserRepository();
  }

  getHashGenerator() {
    return {generate: sha1};
  }

  getUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

}

module.exports = ServiceFactory;
