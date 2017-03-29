
'use strict';

const AWS = require('aws-sdk');
const Uuid = require('uuid');
const bodyParser = require('body-parser');
const express = require('express');
const favicon = require('serve-favicon');
const morgan = require('morgan');
const path = require('path');
const pug = require('pug');
const sha1 = require('sha1');

const ExpressRequestHandler = require('./ExpressRequestHandler');

class ServiceFactory {

  constructor(params) {
    this._env = params.env;
  }

  createAccessLogger() {
    return morgan('dev');
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

  createStaticContentsProvider() {
    return express.static(pathUnderPublic());
  }

  createViewDirectoryPath() {
    return path.join(__dirname, 'views');
  }

  createAuthBasedRedirector() {
    const AuthBasedRedirector = require('./AuthBasedRedirector');
    return new AuthBasedRedirector();
  }

  createGetRootPageRequestHandler() {
    const GetRootPageRequestProcessor = require('./request-processors/GetRootPage');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new GetRootPageRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createGetDonesRequestHandler() {
    const GetDonesRequestProcessor = require('./request-processors/GetDones');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new GetDonesRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createPostDoneRequestHandler() {
    const PostDoneRequestProcessor = require('./request-processors/PostDone');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new PostDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createDeleteDoneRequestHandler() {
    const DeleteDoneRequestProcessor = require('./request-processors/DeleteDone');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new DeleteDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
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

  createGetDonesCommand() {
    const GetDonesCommand = require('./commands/GetDones');
    return new GetDonesCommand();
  }

  createLoginCommand() {
    const LoginCommand = require('./commands/Login');
    return new LoginCommand();
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
    const PostSigninRequestProcessor = require('./request-processors/PostSignin');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new PostSigninRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createSignoutRequestHandler() {
    const SignoutRequestProcessor = require('./request-processors/Signout');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new SignoutRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createNoMatchingRouteRequestHandler() {
    const NoMatchingRouteRequestProcessor = require('./request-processors/NoMatchingRoute');
    const requestHandler = new ExpressRequestHandler({
      requestProcessor: new NoMatchingRouteRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createRequestProcessErrorProcessor() {
    const RequestProcessErrorProcessor = require('./RequestProcessErrorProcessor');
    return new RequestProcessErrorProcessor();
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

  createCookieCodec() {
    const CookieCodec = require('./CookieCodec');
    return new CookieCodec({signatureSecret: this._env.SESSION_SECRET});
  }

  createHtmlPageGenerator() {
    const HtmlPageGenerator = require('./HtmlPageGenerator');
    return new HtmlPageGenerator();
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

  createSessionDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient(this._env.SESSION_TABLE_NAME);
  }

  createSessionRepository() {
    const SessionRepository = require('./repositories/Session');
    return new SessionRepository();
  }

  createHashGenerator() {
    return {generate: sha1};
  }

  createUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

  createDateProvider() {
    return {getCurrentDate: () => new Date()};
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
