
const AWS = require('aws-sdk');
const Uuid = require('uuid');
const bodyParser = require('body-parser');
const cookieParser = require('cookie-parser');
const express = require('express');
const favicon = require('serve-favicon');
const morgan = require('morgan');
const path = require('path');
const sha1 = require('sha1');
const session = require('express-session');

const DynamoDBStore = require('connect-dynamodb')({session});

class ServiceFactory {

  constructor({env}) {
    this._env = env;
  }

  createAccessLogger() {
    return morgan('dev');
  }

  createCookieParser() {
    return cookieParser();
  }

  createEncodedUrlParser() {
    return bodyParser.urlencoded({extended: false});
  }

  createFaviconProvider() {
    return favicon(pathUnderPublic('images/favicon.ico'));
  }

  createJsonRequestBodyParser() {
    return bodyParser.json();
  }

  createSessionManager() {
    return session({
      secret: this._env.SESSION_SECRET,
      saveUninitialized: false,   // don't create session until something stored
      resave: false,              // don't save session if unmodified
      store: new DynamoDBStore({
        table: this._env.SESSION_TABLE_NAME,
        client: this._getDynamoDB()
      })
    });
  }

  createStaticContentsProvider() {
    return express.static(pathUnderPublic());
  }

  createViewDirectoryPath() {
    return path.join(__dirname, 'views');
  }

  createAuthBasedRedirectMiddleware() {
    const AuthBasedRedirectMiddleware = require('./express-middlewares/AuthBasedRedirect');
    return new AuthBasedRedirectMiddleware();
  }

  createSchemaBasedRedirectMiddleware() {
    const SchemaBasedRedirectMiddleware = this._env.NODE_ENV === 'production' ?
        require('./express-middlewares/SchemaBasedRedirect') :
        require('./express-middlewares/DevSchemaBasedRedirect');
    return new SchemaBasedRedirectMiddleware();
  }

  createGetRootPageRequestHandler() {
    const GetRootPageRequestHandler = require('./express-middlewares/GetRootPageRequestHandler');
    return new GetRootPageRequestHandler();
  }

  createGetDonesRequestHandler() {
    const GetDonesRequestHandler = require('./express-middlewares/GetDonesRequestHandler');
    return new GetDonesRequestHandler();
  }

  createPostDonesRequestHandler() {
    const PostDonesRequestHandler = require('./express-middlewares/PostDonesRequestHandler');
    return new PostDonesRequestHandler();
  }

  createDeleteDoneRequestHandler() {
    const DeleteDoneRequestHandler = require('./express-middlewares/DeleteDoneRequestHandler');
    return new DeleteDoneRequestHandler();
  }

  createUpdateDoneRequestHandler() {
    const UpdateDoneRequestHandler = require('./express-middlewares/UpdateDoneRequestHandler');
    return new UpdateDoneRequestHandler();
  }

  createGetSigninRequestHandler() {
    const GetSigninRequestHandler = require('./express-middlewares/GetSigninRequestHandler');
    return new GetSigninRequestHandler();
  }

  createPostSigninRequestHandler() {
    const PostSigninRequestHandler = require('./express-middlewares/PostSigninRequestHandler');
    return new PostSigninRequestHandler();
  }

  createSignoutRequestHandler() {
    const SignoutRequestHandler = require('./express-middlewares/SignoutRequestHandler');
    return new SignoutRequestHandler();
  }

  createNoMatchingRouteRequestHandler() {
    const NoMatchingRouteRequestHandler = require('./express-middlewares/NoMatchingRouteRequestHandler');
    return new NoMatchingRouteRequestHandler();
  }

  createErrorHandler() {
    const ErrorHandler = require('./express-middlewares/ErrorHandler');
    return new ErrorHandler();
  }

  createLogger() {
    return console;
  }

  _getDynamoDB() {
    return new AWS.DynamoDB({region: this._env.DB_REGION});
  }

  createDynamoDBDocumentClient() {
    return new AWS.DynamoDB.DocumentClient({region: this._env.DB_REGION});
  }

  createDoneDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient(this._env.DONE_TABLE_NAME);
  }

  createDoneRepository() {
    const DoneRepository = require('./repositories/Done');
    return new DoneRepository();
  }

  createDoneFormatter() {
    const DoneFormatter = require('./DoneFormatter');
    return new DoneFormatter();
  }

  createUserDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient(this._env.USER_TABLE_NAME);
  }

  createUserRepository() {
    const UserRepository = require('./repositories/User');
    return new UserRepository();
  }

  createHashGenerator() {
    return {generate: sha1};
  }

  createUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

}

// e.g. images/favicon.ico => /path/to/public/images/favicon.ico
function pathUnderPublic(pathFromPublic) {
  const pathParts = pathFromPublic ? pathFromPublic.split('/') : [];
  return path.join(__dirname, '..', '..', 'public', ...pathParts);
}

module.exports = ServiceFactory;
