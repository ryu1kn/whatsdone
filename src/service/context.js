'use strict';

let AWS = require('aws-sdk');

// Singleton class.
// TODO: Change it to non-singleton and inject to classes
//       that need this
class Context {

  setEnv(env) {
    this._env = env;
  }

  getDynamoDB() {
    let env = this._env;
    this._dynamoDB = this._dynamoDB || new AWS.DynamoDB({
      accessKeyId: env.AWS_ACCESS_KEY_ID,
      secretAccessKey: env.AWS_SECRET_ACCESS_KEY,
      region: env.AWS_REGION
    });
    return this._dynamoDB;
  }

  getDynamoDBDocumentClient() {
    let env = this._env;
    this._documentClient = this._documentClient ||
        new AWS.DynamoDB.DocumentClient({
          accessKeyId: env.AWS_ACCESS_KEY_ID,
          secretAccessKey: env.AWS_SECRET_ACCESS_KEY,
          region: env.AWS_REGION
        });
    return this._documentClient;
  }

}

module.exports = new Context();
