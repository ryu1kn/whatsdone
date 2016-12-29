'use strict';

const AWS = require('aws-sdk');

// Singleton class.
// TODO: Change it to non-singleton
class Context {

  setEnv(env) {
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

}

module.exports = new Context();
