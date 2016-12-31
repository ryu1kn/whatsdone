
const AWS = require('aws-sdk');
const Uuid = require('uuid');
const sha1 = require('sha1');

class ServiceFactory {

  constructor({env}) {
    this._env = env;
  }

  getAuthBasedRedirectMiddleware() {
    const AuthBasedRedirectMiddleware = require('./express-middlewares/AuthBasedRedirect');
    this._authBasedRedirectMiddleware = this._authBasedRedirectMiddleware || new AuthBasedRedirectMiddleware();
    return this._authBasedRedirectMiddleware;
  }

  getSchemaBasedRedirectMiddleware() {
    const SchemaBasedRedirectMiddleware = this._env.NODE_ENV === 'production' ?
        require('./express-middlewares/SchemaBasedRedirect') :
        require('./express-middlewares/DevSchemaBasedRedirect');
    this._schemaBasedRedirectMiddleware = this._schemaBasedRedirectMiddleware || new SchemaBasedRedirectMiddleware();
    return this._schemaBasedRedirectMiddleware;
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
    const DoneRepository = require('./repositories/Done');
    return new DoneRepository();
  }

  getUserDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient('users');
  }

  getUserRepository() {
    const UserRepository = require('./repositories/User');
    this._userRepository = this._userRepository || new UserRepository();
    return this._userRepository;
  }

  getHashGenerator() {
    return {generate: sha1};
  }

  getUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

}

module.exports = ServiceFactory;
