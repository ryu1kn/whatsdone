
'use strict';

const AWS = require('aws-sdk');
const Uuid = require('uuid');
const bodyParser = require('body-parser');
const cookieParser = require('cookie-parser');
const express = require('express');
const favicon = require('serve-favicon');
const morgan = require('morgan');
const path = require('path');
const pug = require('pug');
const sha1 = require('sha1');
const session = require('express-session');

const DynamoDBStore = require('connect-dynamodb')({session});
const ExpressRequestHandler = require('./ExpressRequestHandler');

class ServiceFactory {

  constructor(params) {
    this._env = params.env;
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
    return getBoundHandleMethod(new AuthBasedRedirectMiddleware());
  }

  createHttpSchemeBasedRedirectMiddleware() {
    const HttpSchemeBasedRedirectMiddleware = this._env.NODE_ENV === 'production' ?
        require('./express-middlewares/HttpSchemeBasedRedirect') :
        require('./express-middlewares/DevHttpSchemeBasedRedirect');
    return getBoundHandleMethod(new HttpSchemeBasedRedirectMiddleware());
  }

  createGetRootPageRequestHandler() {
    const GetRootPageRequestProcessor = require('./request-processors/GetRootPage');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new GetRootPageRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createGetDonesRequestHandler() {
    const GetDonesRequestHandler = require('./express-middlewares/GetDonesRequestHandler');
    return getBoundHandleMethod(new GetDonesRequestHandler());
  }

  createPostDoneRequestHandler() {
    const PostDoneRequestProcessor = require('./request-processors/PostDone');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new PostDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createDeleteDoneRequestHandler() {
    const DeleteDoneRequestHandler = require('./express-middlewares/DeleteDoneRequestHandler');
    return getBoundHandleMethod(new DeleteDoneRequestHandler());
  }

  createUpdateDoneRequestHandler() {
    const UpdateDoneRequestProcessor = require('./request-processors/UpdateDone');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new UpdateDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createCreateDoneCommand() {
    const CreateDoneCommand = require('./commands/CreateDone');
    return new CreateDoneCommand();
  }

  createUpdateDoneCommand() {
    const UpdateDoneCommand = require('./commands/UpdateDone');
    return new UpdateDoneCommand();
  }

  createGetSigninRequestHandler() {
    const GetSigninRequestProcessor = require('./request-processors/GetSignin');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new GetSigninRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createPostSigninRequestHandler() {
    const PostSigninRequestHandler = require('./express-middlewares/PostSigninRequestHandler');
    return getBoundHandleMethod(new PostSigninRequestHandler());
  }

  createSignoutRequestHandler() {
    const SignoutRequestHandler = require('./express-middlewares/SignoutRequestHandler');
    return getBoundHandleMethod(new SignoutRequestHandler());
  }

  createNoMatchingRouteRequestHandler() {
    const NoMatchingRouteRequestHandler = require('./express-middlewares/NoMatchingRouteRequestHandler');
    return getBoundHandleMethod(new NoMatchingRouteRequestHandler());
  }

  createErrorHandler() {
    const ErrorHandler = require('./express-middlewares/ErrorHandler');
    return getBoundHandleMethod(new ErrorHandler());
  }

  createExpressRequestNormaliser() {
    const ExpressRequestNormaliser = require('./ExpressRequestNormaliser');
    return new ExpressRequestNormaliser();
  }

  createExpressResponseSenderFactory() {
    const ExpressResponseSender = require('./ExpressResponseSender');
    return {create: expressRes => new ExpressResponseSender({expressRes})};
  }

  createLogger() {
    return console;
  }

  createPug() {
    return pug;
  }

  createHtmlPageGenerator() {
    const HtmlPageGenerator = require('./HtmlPageGenerator');
    return new HtmlPageGenerator();
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
  const pathComponents = [__dirname, '..', '..', 'public'].concat(pathParts);
  return path.join.apply(path, pathComponents);
}

function getBoundHandleMethod(handler) {
  return handler.handle.bind(handler);
}

module.exports = ServiceFactory;
