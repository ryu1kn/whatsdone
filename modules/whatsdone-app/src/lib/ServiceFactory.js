
const AWSXRay = require('aws-xray-sdk');
const AWS = AWSXRay.captureAWS(require('aws-sdk'));
const Uuid = require('uuid');

const LambdaRequestHandler = require('./LambdaRequestHandler');

class ServiceFactory {

  constructor(params) {
    this._env = params.env;
  }

  createConfig() {
    return {
      userPoolId: process.env.USER_POOL_ID,
      webappOrigin: process.env.WEBAPP_ORIGIN
    };
  }

  createCognitoIdentityServiceProvider() {
    return new AWS.CognitoIdentityServiceProvider();
  }

  createGetDonesRequestHandler() {
    const GetDonesRequestProcessor = require('./request-processors/GetDones');
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new GetDonesRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createPostDoneRequestHandler() {
    const PostDoneRequestProcessor = require('./request-processors/PostDone');
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new PostDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createDeleteDoneRequestHandler() {
    const DeleteDoneRequestProcessor = require('./request-processors/DeleteDone');
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new DeleteDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createUpdateDoneRequestHandler() {
    const UpdateDoneRequestProcessor = require('./request-processors/UpdateDone');
    const requestHandler = new LambdaRequestHandler({
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

  createUpdateDoneCommand() {
    const UpdateDoneCommand = require('./commands/UpdateDone');
    return new UpdateDoneCommand();
  }

  createPostSigninRequestHandler() {
    const PostSigninRequestProcessor = require('./request-processors/PostSignin');
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new PostSigninRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createSignoutRequestHandler() {
    const SignoutRequestProcessor = require('./request-processors/Signout');
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new SignoutRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createRequestProcessErrorProcessor() {
    const RequestProcessErrorProcessor = require('./RequestProcessErrorProcessor');
    return new RequestProcessErrorProcessor();
  }

  createLambdaRequestNormaliser() {
    const LambdaRequestNormaliser = require('./LambdaRequestNormaliser');
    return new LambdaRequestNormaliser();
  }

  createLambdaResponseFormatter() {
    const LambdaResponseFormatter = require('./LambdaResponseFormatter');
    return new LambdaResponseFormatter();
  }

  createLogger() {
    return console;
  }

  createDynamoDBDocumentClient() {
    return new AWS.DynamoDB.DocumentClient({region: this._env.DB_REGION});
  }

  createDoneQueryHelper() {
    const DoneQueryHelper = require('./repositories/done-helpers/query');
    return new DoneQueryHelper(this._env.DONE_TABLE_NAME);
  }

  createDoneDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient({collectionName: this._env.DONE_TABLE_NAME, idName: 'id'});
  }

  createDoneRepository() {
    const DoneRepository = require('./repositories/Done');
    return new DoneRepository();
  }

  createUserDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient');
    return new DynamoTableClient({collectionName: this._env.USER_TABLE_NAME, idName: 'id'});
  }

  createUserNameService() {
    const UserNameService = require('./UserNameService');
    return new UserNameService();
  }

  createUserIdRepository() {
    const UserIdRepository = require('./repositories/UserId');
    return new UserIdRepository({tableName: this._env.USER_ID_TABLE_NAME});
  }

  createUuidGenerator() {
    return {generate: () => Uuid.v4()};
  }

  createDateProvider() {
    return {getCurrentDate: () => new Date()};
  }

}

function getBoundHandleMethod(handler) {
  return handler.handle.bind(handler);
}

module.exports = ServiceFactory;
