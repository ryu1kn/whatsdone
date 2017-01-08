
const AWS = require('aws-sdk');
const Uuid = require('uuid');
const bodyParser = require('body-parser');
const cookieParser = require('cookie-parser');
const express = require('express');
const morgan = require('morgan');
const path = require('path');
const sha1 = require('sha1');
const session = require('express-session');
const DynamoDBStore = require('connect-dynamodb')({session});

class ServiceFactory {

  constructor({env}) {
    this._env = env;
  }

  getAccessLogger() {
    this._accessLogger = this._accessLogger || morgan('dev');
    return this._accessLogger;
  }

  getCookieParser() {
    this._cookieParser = this._cookieParser || cookieParser();
    return this._cookieParser;
  }

  getEncodedUrlParser() {
    this._encodedUrlParser = this._encodedUrlParser || bodyParser.urlencoded({extended: false});
    return this._encodedUrlParser;
  }

  getJsonRequestBodyParser() {
    this._jsonRequestBodyParser = this._jsonRequestBodyParser || bodyParser.json();
    return this._jsonRequestBodyParser;
  }

  getSessionManager() {
    this._sessionManager = this._sessionManager || session({
      secret: this._env.SESSION_SECRET,
      saveUninitialized: false,   // don't create session until something stored
      resave: false,              // don't save session if unmodified
      store: new DynamoDBStore({
        table: 'whatsdone-sessions',
        client: this._getDynamoDB()
      })
    });
    return this._sessionManager;
  }

  getStaticContentsProvider() {
    this._staticContentsProvider = this._staticContentsProvider || express.static(path.join(__dirname, '..', '..', 'public'));
    return this._staticContentsProvider;
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

  getGetRootPageRequestHandler() {
    const GetRootPageRequestHandler = require('./express-middlewares/GetRootPageRequestHandler');
    this._getRootPageRequestHandler = this._getRootPageRequestHandler || new GetRootPageRequestHandler();
    return this._getRootPageRequestHandler;
  }

  getGetDonesRequestHandler() {
    const GetDonesRequestHandler = require('./express-middlewares/GetDonesRequestHandler');
    this._getDonesRequestHandler = this._getDonesRequestHandler || new GetDonesRequestHandler();
    return this._getDonesRequestHandler;
  }

  getPostDonesRequestHandler() {
    const PostDonesRequestHandler = require('./express-middlewares/PostDonesRequestHandler');
    this._postDonesRequestHandler = this._postDonesRequestHandler || new PostDonesRequestHandler();
    return this._postDonesRequestHandler;
  }

  getDeleteDoneRequestHandler() {
    const DeleteDoneRequestHandler = require('./express-middlewares/DeleteDoneRequestHandler');
    this._deleteDoneRequestHandler = this._deleteDoneRequestHandler || new DeleteDoneRequestHandler();
    return this._deleteDoneRequestHandler;
  }

  getUpdateDoneRequestHandler() {
    const UpdateDoneRequestHandler = require('./express-middlewares/UpdateDoneRequestHandler');
    this._updateDoneRequestHandler = this._updateDoneRequestHandler || new UpdateDoneRequestHandler();
    return this._updateDoneRequestHandler;
  }

  getGetSigninRequestHandler() {
    const GetSigninRequestHandler = require('./express-middlewares/GetSigninRequestHandler');
    this._getSigninRequestHandler = this._getSigninRequestHandler || new GetSigninRequestHandler();
    return this._getSigninRequestHandler;
  }

  getPostSigninRequestHandler() {
    const PostSigninRequestHandler = require('./express-middlewares/PostSigninRequestHandler');
    this._postSigninRequestHandler = this._postSigninRequestHandler || new PostSigninRequestHandler();
    return this._postSigninRequestHandler;
  }

  getSignoutRequestHandler() {
    const SignoutRequestHandler = require('./express-middlewares/SignoutRequestHandler');
    this._signoutRequestHandler = this._signoutRequestHandler || new SignoutRequestHandler();
    return this._signoutRequestHandler;
  }

  getNoMatchingRouteRequestHandler() {
    const NoMatchingRouteRequestHandler = require('./express-middlewares/NoMatchingRouteRequestHandler');
    this._noMatchingRouteRequestHandler = this._noMatchingRouteRequestHandler || new NoMatchingRouteRequestHandler();
    return this._noMatchingRouteRequestHandler;
  }

  getErrorHandler() {
    const ErrorHandler = require('./express-middlewares/ErrorHandler');
    this._errorHandler = this._errorHandler || new ErrorHandler();
    return this._errorHandler;
  }

  getLogger() {
    this._logger = this._logger || console;
    return this._logger;
  }

  _getDynamoDB() {
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
    this._doneRepository = this._doneRepository || new DoneRepository();
    return this._doneRepository;
  }

  getDoneFormatter() {
    const DoneFormatter = require('./DoneFormatter');
    this._doneFormatter = this._doneFormatter || new DoneFormatter();
    return this._doneFormatter;
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
