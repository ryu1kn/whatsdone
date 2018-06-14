import AWSXRay = require('aws-xray-sdk');
import RawAWS = require('aws-sdk');
import LambdaRequestHandler from './LambdaRequestHandler';
import Uuid = require('uuid');
import {ObjectMap} from './models/Collection';

const AWS = AWSXRay.captureAWS(RawAWS);

export default class ServiceFactory {
  private _env: ObjectMap<string>;

  constructor(env: ObjectMap<string>) {
    this._env = env;
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

  createCognitoUserFinder() {
    const CognitoUserFinder = require('./CognitoUserFinder').default;
    return new CognitoUserFinder();
  }

  createGetDonesRequestHandler() {
    const GetDonesRequestProcessor = require('./request-processors/GetDones').default;
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new GetDonesRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createPostDoneRequestHandler() {
    const PostDoneRequestProcessor = require('./request-processors/PostDone').default;
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new PostDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createDeleteDoneRequestHandler() {
    const DeleteDoneRequestProcessor = require('./request-processors/DeleteDone').default;
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new DeleteDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createUpdateDoneRequestHandler() {
    const UpdateDoneRequestProcessor = require('./request-processors/UpdateDone').default;
    const requestHandler = new LambdaRequestHandler({
      requestProcessor: new UpdateDoneRequestProcessor()
    });
    return getBoundHandleMethod(requestHandler);
  }

  createCreateDoneCommand() {
    const CreateDoneCommand = require('./commands/CreateDone').default;
    return new CreateDoneCommand();
  }

  createGetDonesCommand() {
    const GetDonesCommand = require('./commands/GetDones').default;
    return new GetDonesCommand();
  }

  createUpdateDoneCommand() {
    const UpdateDoneCommand = require('./commands/UpdateDone').default;
    return new UpdateDoneCommand();
  }

  createRequestProcessErrorProcessor() {
    const RequestProcessErrorProcessor = require('./RequestProcessErrorProcessor').default;
    return new RequestProcessErrorProcessor();
  }

  createLambdaRequestNormaliser() {
    const LambdaRequestNormaliser = require('./LambdaRequestNormaliser').default;
    return new LambdaRequestNormaliser();
  }

  createLambdaResponseFormatter() {
    const LambdaResponseFormatter = require('./LambdaResponseFormatter').default;
    return new LambdaResponseFormatter();
  }

  createLogger(): {error: (...args: any[]) => void} {
    return console;
  }

  createDynamoDBDocumentClient(): AWS.DynamoDB.DocumentClient {
    return new AWS.DynamoDB.DocumentClient({region: this._env.DB_REGION});
  }

  createDoneQueryHelper() {
    const DoneQueryHelper = require('./repositories/done-helpers/query').default;
    return new DoneQueryHelper(this._env.DONE_TABLE_NAME);
  }

  createDoneDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient').default;
    return new DynamoTableClient({collectionName: this._env.DONE_TABLE_NAME, idName: 'id'});
  }

  createDoneRepository() {
    const DoneRepository = require('./repositories/Done').default;
    return new DoneRepository();
  }

  createUserNameService() {
    const UserNameService = require('./UserNameService').default;
    return new UserNameService();
  }

  createUserIdRepository() {
    const UserIdRepository = require('./repositories/UserId').default;
    return new UserIdRepository({tableName: this._env.USER_ID_TABLE_NAME});
  }

  createUuidGenerator(): {generate: () => string} {
    return {generate: () => Uuid.v4()};
  }

  createDateProvider(): {getCurrentDate: () => Date} {
    return {getCurrentDate: () => new Date()};
  }

}

function getBoundHandleMethod(handler) {
  return handler.handle.bind(handler);
}
